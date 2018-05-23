import shutil
import argparse
import os
from subprocess import Popen

parser = argparse.ArgumentParser(description='Build and publish Oasis ARA.')
parser.add_argument(
    '-g', '--github_uname', metavar='N', type=str, required=True,
    help='The username for GitHub.')
parser.add_argument(
    '-G', '--github_password', metavar='N', type=str, required=True,
    help='The password for GitHub.')
parser.add_argument(
    '-d', '--docker_uname', type=str, default='',
    help="The username for DockerHub.")
parser.add_argument(
    '-D', '--docker_password', type=str, default='',
    help="The password for DockerHub.")
parser.add_argument(
    '-r', '--release_tag', type=str,  default='',
    help="The release tag.")
parser.add_argument(
    '-b', '--build', action='store_true',
    help='Checkout of github and build.')
parser.add_argument(
    '-c', '--clean', action='store_true',
    help='Clean all Docker images and containers.')
parser.add_argument(
    '-p', '--publish', action='store_true',
    help='Publish to DockerHub.')
# parser.add_argument(
#     '-t', '--integration_tests', action='store_true',
#     help='Run integration tests.')

args = parser.parse_args()

github_uname = args.github_uname 
github_password = args.github_password
docker_uname = args.docker_uname
docker_password = args.docker_password
release_tag = args.release_tag
do_clean_docker = args.clean
do_publish = args.publish
#do_integration_tests = args.integration_tests
do_build = args.build

do_github_tags = True
do_docker_tags = True

def run_command(desc, cmd, exit_on_fail=True, retry=False):
    proc = Popen(cmd, shell=True)
    proc.wait()
    if (proc.returncode > 0 and exit_on_fail):
        print ("FAIL: {}".format(desc))
        if retry:
            print("RETRY: {}".format(desc))
            run_command(desc, cmd, True, False)
        else:
            exit(255)

if do_clean_docker:
    # run_command(
    #     "Stop all running docker containers",
    #     "docker ps -aq | xargs docker rm -f", False)

    # run_command(
    #     "Delete all docker images",
    #     "docker images -q | xargs docker rmi -f", False)

    run_command(
        "Remove existing directories",
        "rm -rf flamingo")

    run_command(
        "Clone flamingo",
        "git clone --recursive https://{}:{}@github.com/oasislmf/flamingo.git".format(
            github_uname, github_password))

if do_build:

    os.chdir("flamingo")
    run_command(
        "Build flamingo server image",
        "docker build --no-cache=true -t flamingo_server -f Dockerfile.flamingo_server .")
    run_command(
        "Build flamingo shiny image",
        "docker build --no-cache=true -t flamingo_shiny_proxy -f Dockerfile.shiny_proxy .")
    run_command(
        "Build flamingo shiny image",
        "docker build --no-cache=true -t flamingo_shiny -f Dockerfile.flamingo_shiny .")
    
    if do_github_tags:
        run_command(
            "Tag flamingo repo",
            "git tag -f {}".format(release_tag))        
        run_command(
            "Push flamingos tag",
            "git push origin {}".format(release_tag))        

    os.chdir("..")

run_command(
    "Tag flamingo_server image",
    "docker tag flamingo_server coreoasis/flamingo_server:{}".format(release_tag))
run_command(
    "Tag flamingo_shiny_proxy image",
    "docker tag flamingo_shiny_proxy coreoasis/shiny_proxy:{}".format(release_tag))
run_command(
    "Tag flamingo_shiny image",
    "docker tag flamingo_shiny coreoasis/flamingo_shiny:{}".format(release_tag))

if do_publish:
    run_command(
        "Login to DockerHub",
    	"docker login -p {} -u {}".format(docker_password, docker_uname),
        True)
    run_command(
        "Push flamingo_server to DockerHub",
    	"docker push coreoasis/flamingo_server:{}".format(release_tag),
        True)
    run_command(
        "Push shiny_proxy to DockerHub",
    	"docker push coreoasis/shiny_proxy:{}".format(release_tag),
        True)        
    run_command(
        "Push flamingo_shiny to DockerHub",
    	"docker push coreoasis/flamingo_shiny:{}".format(release_tag),
        True)
