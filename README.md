# kt

This repo provides a docker image as a tool to use `gomplate` with `kubectl` to deploy Kubernetes apps across different environments.

## Usage

To use this tool, add it to your `docker-compose.yml` file as the following service:

```yaml
services:
  kt:
    image: myobplatform/kt
    volumes:
      - "$HOME/.kube:/root/.kube"
      - ".:/app"
```

You can now run the tool as required with `docker-compose run --rm kt <command>` where `<command>` is one of the following:

* *validate*: Compiles the templates with a given env (given via the `-e ENV` flag) and will validate the validity of the compiles manifests againsts the Kubernetes API server.
* *deploy*: Will compile and deploy the manifests files for an environment (given via the `-e ENV` flag).

The above are the two main commands, but because `kt` uses a dependency build tool you can debug intermin stages of the build such:

* *compile*: Will compile each template file only with the given env - you can then view them in the `_build/compiled` folder.
* *join*: Will compile and then merge all the template files together into a single file of Kubernetes manifests. This can be viewed at `_build/<env>/joined.yaml`.

## Conventions

The `kt` tool assumes the following conventions of your project:

* You put your `gomplate` files in the `templates` folder. You can create sub folders under that to arbitrary depth.
* You put the environment files, AKA the `gomplate` datasource files in the `envs` folder and name each file after the environment.

## Development

`kt` is simply a combination of the following tools with folder conventions:

* [shake](http://shakebuild.com/) to provide a programmatic dependency build tool specific to our conventions and domain. This is a library in Haskell so gives full programming language power, with strong type safety.
* [gomplate](https://gomplate.hairyhenderson.ca/) to provide templating of Kubernetes manifests to allow for different environments and complex setup
* [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/) to due the actual deployment of the compiled template manifests.

This is sticking with the principle of not reinventing the wheel and rewriting a specific tool from scratch that would require more effort, testing and maintenance.

### Roadmap

Assuming this tool is useful here are some extra features that could be implemented in the future:

* Manage the lifecycle of objects by using a label for each project to be able to see if any objects need to be deleted as they are present on the API server but no longer present in manifest files.
