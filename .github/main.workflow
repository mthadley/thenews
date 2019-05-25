workflow "Main workflow" {
  on = "push"
  resolves = ["deploy"]
}

action "build" {
  uses = "docker://node:10"
  args = "make all test"
  env = {
    ENVIRONMENT = "production"
  }
}

action "is-branch-master" {
  needs = "build"
  uses = "actions/bin/filter@master"
  args = "branch master"
}

action "deploy" {
  needs = "is-branch-master"
  uses = "peaceiris/actions-gh-pages@v1.0.0"
  env = {
    PUBLISH_DIR = "./dist"
    PUBLISH_BRANCH = "gh-pages"
  }
  secrets = ["ACTIONS_DEPLOY_KEY"]
}
