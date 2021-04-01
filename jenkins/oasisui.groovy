node {
    hasFailed = false
    sh 'sudo /var/lib/jenkins/jenkins-chown'
    deleteDir() // wipe out the workspace

    properties([
      parameters([
        [$class: 'StringParameterDefinition',  name: 'BUILD_BRANCH', defaultValue: 'master'],
        [$class: 'StringParameterDefinition',  name: 'SOURCE_BRANCH', defaultValue: BRANCH_NAME],
        [$class: 'StringParameterDefinition',  name: 'RELEASE_TAG', defaultValue: BRANCH_NAME.split('/').last() + "-${BUILD_NUMBER}"],
        [$class: 'StringParameterDefinition',  name: 'BASE_TAG', defaultValue: 'latest'],
        [$class: 'BooleanParameterDefinition', name: 'PURGE', value: Boolean.valueOf(true)],
        [$class: 'BooleanParameterDefinition', name: 'PUBLISH', value: Boolean.valueOf(false)],
        [$class: 'BooleanParameterDefinition', name: 'PRE_RELEASE', value: Boolean.valueOf(true)],
        [$class: 'BooleanParameterDefinition', name: 'AUTO_MERGE', defaultValue: Boolean.valueOf(true)],
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

    proxy_docker="docker/Dockerfile.oasisui_proxy"
    proxy_image="coreoasis/oasisui_proxy"

    app_docker="docker/Dockerfile.oasisui_app"
    app_image="coreoasis/oasisui_app"

     //make sure release candidate versions are tagged correctly                                                                              
     if (params.PUBLISH && params.PRE_RELEASE && ! params.RELEASE_TAG.matches("^(\\d+\\.)(\\d+\\.)(\\*|\\d+)rc(\\d+)$")) { 
         sh "echo release candidates must be tagged {version}rc{N}, example: 1.0.0rc1"
         sh "exit 1"
     } 

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
                                sh "git fetch origin pull/$CHANGE_ID/head:$BRANCH_NAME"
                                sh "git checkout $CHANGE_TARGET"
                                sh "git merge $BRANCH_NAME"
                                app_branch = CHANGE_BRANCH
                                // WARNING: this will fail for external pull requests

                            } else {
                                // Checkout branch
                                sh "git checkout ${source_branch}"
                                app_branch = source_branch
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
                        sh PIPELINE + " build_image  ${proxy_docker}  ${proxy_image} ${env.TAG_RELEASE}"
                    }
                }
            },
            clone_app: {
                stage('Build: Shiny App') {
                    dir(source_workspace) {
                        sh "docker build --no-cache -f ${app_docker} --pull --build-arg REF_BRANCH=${app_branch} -t ${app_image} -t ${app_image}:${env.TAG_RELEASE} ."
                    }
                }
            }
        )

        // ToDO add testing here
        //stage('Run Oasisui') {
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
                            sh PIPELINE + " push_image ${proxy_image} ${env.TAG_RELEASE}"
                            if (! params.PRE_RELEASE){
                                sh PIPELINE + " push_image ${proxy_image} latest"
                            }   
                        }
                    }
                 },
                 publish_app: {
                    stage ('Publish: Shiny App') {
                        dir(source_workspace) {
                            sh PIPELINE + " push_image ${app_image} ${env.TAG_RELEASE}"
                            if (! params.PRE_RELEASE){
                                sh PIPELINE + " push_image ${app_image} latest"
                            }   
                        }
                    }
                 }
            )

            //Git Tag
            sshagent (credentials: [git_creds]) {
                dir(source_workspace) {
                    sh "git tag ${env.TAG_RELEASE}"
                    sh "git push origin ${env.TAG_RELEASE}"
                }
            }

            // Create Github release 
            withCredentials([string(credentialsId: 'github-api-token', variable: 'gh_token')]) {
                String repo = "OasisLMF/OasisUI"
                def json_request = readJSON text: '{}'
                json_request['tag_name'] = RELEASE_TAG
                json_request['target_commitish'] = 'master'
                json_request['name'] = RELEASE_TAG
                json_request['body'] = ""
                json_request['draft'] = false
                json_request['prerelease'] = params.PRE_RELEASE
                writeJSON file: 'gh_request.json', json: json_request
                sh 'curl -XPOST -H "Authorization:token ' + gh_token + "\" --data @gh_request.json https://api.github.com/repos/$repo/releases > gh_response.json"
            }
        }
    } catch(hudson.AbortException | org.jenkinsci.plugins.workflow.steps.FlowInterruptedException buildException) {
        hasFailed = true
        error('Build Failed')
    } finally {

        //Docker cleanup
        dir(build_workspace) {
            sh PIPELINE + " purge_image ${proxy_image} ${env.TAG_RELEASE}"
            sh PIPELINE + " purge_image ${app_image} ${env.TAG_RELEASE}"
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

        // Run merge back if publish
        if (params.PUBLISH && params.AUTO_MERGE){ 
            dir(source_workspace) {
                sshagent (credentials: [git_creds]) {
                    sh "git stash"
                    sh "git checkout master && git pull"
                    sh "git merge ${source_branch} && git push"
                    sh "git checkout develop && git pull"
                    sh "git merge master && git push"
                }   
            }   
        }   
    }
}
