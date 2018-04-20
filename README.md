[![Build Status](https://travis-ci.org/MYOB-Technology/kt.svg?branch=master)](https://travis-ci.org/MYOB-Technology/kt)

# kt

This repo provides a docker image as a tool to use `gomplate` with `kubectl` to deploy Kubernetes apps across different environments with templating.

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
* *delete*: **CAUTION**, will compile and join the manifests and then delete all the Objects on the API server that are named in the compiled manifests.

### Command line flags

`kt` has two flags to use when running the commands above:

* -e ENVIRONMENT  The Kubernetes environment to deploy to (name of file in 'env' folder sans .yaml).
* -c COMPONENT  The component (a subfolder under your templates dir) you want to deploy.

## Conventions

The `kt` tool assumes the following conventions of your project:

* You put your `gomplate` files in the `templates` folder. You can create sub folders under that to arbitrary depth.
* You put the environment files, AKA the `gomplate` datasource files in the `envs` folder and name each file after the environment.

## Templating

The templating available to you for files in the `templates` folder is using the gomplate cli tool, so visit [their docs](https://gomplate.hairyhenderson.ca/syntax/) for a list of templating functions available to you.

On top of the gomplate functions, `kt` adds in the following additional power that is specific to creating Kubernetes manifest templates:

* Reference `datasource "config"` in a template to pull out the values that you specify in your `envs` files, eg `{{ $config := (datasource "config") }}` will give you a variable that you can access for env values using dot notation such as `{{ $config.envValue }}` where `envValue` is the YAML key in each of your `envs` files.
* You are able to access all other template contents from a template using `gomplate`'s `file.Read` function. just specify the relative path from the root folder of the project as the relative path, eg `{{ file.Read "templates/mycomponent/config-map.yaml" }}` to access the contents of the `templates/mycomponent/config-map.yaml` file. _NOTE: To avoid circular dependencies and other issues, note that including any other template will be done without any template compilation - it is the raw file from disk._

### Ordering

The template files are joined in alphabetical order. This means that one can control the order in which objects are applied to the Kubernetes API server by simply prefixing files with numbers to force the ordering.

## Development

`kt` is simply a combination of the following tools with folder conventions:

* [gomplate](https://gomplate.hairyhenderson.ca/) to provide templating of Kubernetes manifests to allow for different environments and complex setup
* [kubectl](https://kubernetes.io/docs/tasks/tools/install-kubectl/) to due the actual deployment of the compiled template manifests.

By using pre made tools such as gomplate we get alot of useful extras already built in, such as extra functions for templating and a great dependency build tool that supports parallelisation and command line flags with zero effort or maintenance.

This is sticking with the principle of not reinventing the wheel and rewriting a specific tool from scratch that would require more effort, testing and maintenance. Were `kt` to be useful and we found that we were fighting the above tools, or having to bend over backwards too much to have it work with them, this is when we would consider writing a tool from scratch.


### Roadmap

Assuming this tool is useful here are some extra features that could be implemented in the future:

* Manage the lifecycle of objects by using a label for each project to be able to see if any objects need to be deleted as they are present on the API server but no longer present in manifest files.
