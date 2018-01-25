# ops-kube-template

This repo provides a docker image as a tool to use `gomplate` with `kubectl` to deploy Kubernetes apps across different environments.

## Usage

To use this tool, add it to your `docker-compose.yml` file as the following service:

```yaml
services:
  t:
    image: myobplatform/kube-template
    volumes:
      - "$HOME/.kube:/root/.kube"
      - ".:/app"
```

You can now run the tool as required with `docker-compose run --rm t <command>` where `<command>` is one of the following:

TBD

## Conventions

The `kube-template` tool assumes the following conventions of your project:

* You put your `gomplate` files in the `templates` folder. You can create sub folders under that to arbitrary depth.
* You put the environment files, AKA the `gomplate` datasource files in the `envs` folder and name each file after the environment.

## Development

`kube-template` is simply a combining of the following tools with folder conventions:

* [shake](http://shakebuild.com/) to provide a programmatic dependency build tool specific to our conventions and domain. This is a library in Haskell so gives full programming language power, with strong type safety.
* [gomplate](https://gomplate.hairyhenderson.ca/) to provide templating of Kubernetes manifests to allow for different environments and complex setup
* [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/) to due the actual deployment of the compiled template manifests.
