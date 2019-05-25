workflow "Main workflow" {
  on = "push"
  resolves = ["build"]
}

action "build" {
  uses = "docker://node:10"
  args = "make all test"
}
