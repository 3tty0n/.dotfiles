function docker_rm_all
  docker rm (docker ps -aq)
end
