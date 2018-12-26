.RECIPEPREFIX := > 
SHELL := /bin/bash
git_repo_root ?= $(PWD)/..
local_bin_path ?= $(HOME)/.local/bin
REPO := 019208955384.dkr.ecr.us-east-1.amazonaws.com/xss-sanitize
TAG ?= latest

default: docker-build
#######################################
# building stack project using docker #
#######################################

# pull stack lts docker image
.PHONY: fpco-image-pull
fpco-image-pull:
> stack docker pull

# stack install and test using docke
.PHONY: docker-install
docker-install: fpco-image-pull
> mkdir -p dockerbin
> stack install --docker --local-bin-path=dockerbin

############################
# building our docker image#
############################
# build our docker image locally
.PHONY: docker-build
docker-build: docker-install
> docker build -t $(REPO):$(TAG) .

# push our local docker image to repository
.PHONY: docker-push
docker-push: docker-build
> aws ecr get-login --no-include-email | sh
> docker push $(REPO):$(TAG)

# delete the local image to save space
.PHONY: docker-remove
docker-remove:
> docker rmi $(REPO):$(TAG)


##################################
# building stack project locally #
##################################

# build, test and install the project using docker
.PHONY: build
build: test install

# 2 - run tests using special script
.PHONY: test
test:
> stack test

# 3 - stack install and test using docker
# TODO run tests by default in Jenkins and fail if tests fails
.PHONY: install
install:
> stack install --local-bin-path="$(local_bin_path)"

# cleaning stack
.PHONY: clean
clean:
> stack clean --full
