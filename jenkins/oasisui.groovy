node {
    hasFailed = false
    sh 'sudo /var/lib/jenkins/jenkins-chown'
    deleteDir() // wipe out the workspace

    properties([
      parameters([
        [$class: 'StringParameterDefinition',  name: 'BUILD_BRANCH', defaultValue: 'master'],
        [$class: 'StringParameterDefinition',  name: 'SOURCE_BRANCH', defaultValue: BRANCH_NAME],
        [$class: 'StringParameterDefinition',  name: 'RELEASE_TAG', defaultValue: "${BRANCH_NAME}-${BUILD_NUMBER}"],
        [$class: 'StringParameterDefinition',  name: 'BASE_TAG', defaultValue: 'latest'],
        [$class: 'BooleanParameterDefinition', name: 'PURGE', value: Boolean.valueOf(false)],
        [$class: 'BooleanParameterDefinition', name: 'PUBLISH', value: Boolean.valueOf(false)],
        [$class: 'BooleanParameterDefinition', name: 'SLACK_MESSAGE', value: Boolean.valueOf(false)]
      ])
    ])


    // Build vars
    String build_repo = 'git@github.com:OasisLMF/build.git'
    String build_branch = params.BUILD_BRANCH
    String build_workspace = 'oasis_build'
    String script_dir = env.WORKSPACE + "/" + build_workspace
    String PIPELINE = script_dir + "/buildscript/pipeline.sh"
    String git_creds = "1335b248-336a-47a9-b0f6-9f7314d6f1f4"

    String source_branch    = params.SOURCE_BRANCH  // Git repo branch to build from
    String source_name      = 'OasisUI'
    String source_git_url   = "git@github.com:OasisLMF/${source_name}.git"
    String source_workspace = "ui_workspace"
    String source_sh        = '/buildscript/utils.sh'


    //env.PYTHON_ENV_DIR = "${script_dir}/pyth-env"           // Virtualenv location
    env.PIPELINE_LOAD =  script_dir + source_sh             // required for pipeline.sh calls
    sh 'env'


    // Set Global ENV
    env.TAG_BASE         = params.BASE_TAG                  // Build TAG for base set of images
    env.TAG_RELEASE      = params.RELEASE_TAG               // Build TAG for TARGET image
    env.TAG_RUN_PLATFORM = env.TAG_BASE                     // Version of Oasis Platform to use for testing
    env.COMPOSE_PROJECT_NAME = UUID.randomUUID().toString().replaceAll("-","")

    docker_proxy="docker/Dockerfile.oasisui_proxy"
    image_proxy="coreoasis/oasisui_proxy"

    docker_app="docker/Dockerfile.oasisui_app"
    image_app="coreoasis/oasisui_app"



    try {
        // CLONE REPOS
        parallel(
            clone_build: {
                stage('Clone: ' + build_workspace) {
                    dir(build_workspace) {
                       git url: build_repo, credentialsId: git_creds, branch: build_branch
                    }
                }
            },
            clone_source: {
                stage('Clone: ' + source_name) {
                    sshagent (credentials: [git_creds]) {
                        dir(source_workspace) {
                            sh "git clone --recursive ${source_git_url} ."
                            if (source_branch.matches("PR-[0-9]+")){
                                // Checkout PR and merge into target branch, test on the result
                                sh "git fetch origin pull/$CHANGE_ID/head:$BRANCH_NAME"
                                sh "git checkout $BRANCH_NAME"
                                sh "git format-patch $CHANGE_TARGET --stdout > ${BRANCH_NAME}.patch"
                                sh "git checkout $CHANGE_TARGET"
                                sh "git apply --stat ${BRANCH_NAME}.patch"  // Print files changed
                                sh "git apply --check ${BRANCH_NAME}.patch" // Check for merge conflicts
                                sh "git apply ${BRANCH_NAME}.patch"         // Apply the patch
                            } else {
                                // Checkout branch
                                sh "git checkout -b ${source_branch}"
                            }
                        }
                    }
                }
            }
        )

        // DOCKER BUILD
        parallel(
            build_proxy: {
                stage('Build: Shiny Proxy') {
                    dir(source_workspace) {
                        sh PIPELINE + " build_image  ${docker_proxy}  ${image_proxy} ${env.TAG_RELEASE}"
                    }
                }
            },
            clone_app: {
                stage('Build: Shiny App') {
                    dir(source_workspace) {
                        sh PIPELINE + " build_image  ${docker_app}  ${image_app} ${env.TAG_RELEASE}"
                    }
                }
            }
        )

        // ToDO add testing here
        //stage('Run Flamingo') {
        //    dir('oasis_build') {
        //        sh PIPELINE + " run_ui"
        //    }
        //}

        //Optionaly Publish to docker hub stage
        if (params.PUBLISH){
            parallel(
                publish_proxy: {

                    stage ('Publish: Shiny Proxy') {
                        dir(source_workspace) {
                            sh PIPELINE + " push_image ${image_proxy} ${env.TAG_RELEASE}"
                        }
                    }
                 },
                 publish_app: {
                    stage ('Publish: Shiny App') {
                        dir(source_workspace) {
                            sh PIPELINE + " push_image ${image_app} ${env.TAG_RELEASE}"
                        }
                    }
                 }
            )
        }
    } catch(hudson.AbortException | org.jenkinsci.plugins.workflow.steps.FlowInterruptedException buildException) {
        hasFailed = true
        error('Build Failed')
    } finally {
        //Docker cleanup
        dir(build_workspace) {
            if(params.PURGE){
                sh PIPELINE + " purge_image ${image_proxy} ${env.TAG_RELEASE}"
                sh PIPELINE + " purge_image ${image_app} ${env.TAG_RELEASE}"
            }
        }

        //Notify Slack
        if(params.SLACK_MESSAGE && (params.PUBLISH || hasFailed)){
            def slackColor = hasFailed ? '#FF0000' : '#27AE60'
            SLACK_GIT_BRANCH = params.SOURCE_BRANCH
            SLACK_GIT_REPO = 'OasisUI'
            SLACK_GIT_URL = "https://github.com/OasisLMF/${SLACK_GIT_REPO}/tree/${SLACK_GIT_BRANCH}"
            SLACK_MSG = "*${env.JOB_NAME}* - (<${env.BUILD_URL}|${env.RELEASE_TAG}>): " + (hasFailed ? 'FAILED' : 'PASSED')
            SLACK_MSG += "\nBranch: <${SLACK_GIT_URL}|${SLACK_GIT_BRANCH}>"
            SLACK_MSG += "\nMode: " + (params.PUBLISH ? 'Publish' : 'Build Test')
            SLACK_CHAN = (params.PUBLISH ? "#builds-release":"#builds-dev")
            slackSend(channel: SLACK_CHAN, message: SLACK_MSG, color: slackColor)
        }
        //Git Tagging
        if(! hasFailed && params.PUBLISH){
            sshagent (credentials: [git_creds]) {
                dir(source_workspace) {
                    sh "git tag ${env.TAG_RELEASE}"
                    sh "git push origin ${env.TAG_RELEASE}"
                }
            }
        }
        //Store logs
        //dir('oasis_build') {
        //    archiveArtifacts artifacts: 'stage/log/**/*.*', excludes: '*stage/log/**/*.gitkeep'
        //}
    }
}
