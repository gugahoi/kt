#! /bin/bash

set -eo pipefail

progName=$(basename $0)
componentBuildPath="_all"
componentTemplatePath=""
env=""

parse_args() {
  while getopts ":e:c:j:" opt; do
    case $opt in
      e)
        if [[ "$OPTARG" == "" ]]; then
          echo "-e needs an environment" >&2
          exit 1
        fi

        env=$OPTARG
        ;;
      c)
        if [[ "$OPTARG" == "" ]]; then
          echo "-c needs a template folder (component)" >&2
          exit 1
        fi

        componentTemplatePath=$OPTARG
        componentBuildPath=$OPTARG
        ;;
      j)
        echo "WARNING: the -j flag is deprecated and does not do anything." >&2
        ;;
      \?)
        echo "Invalid argument passed: -$OPTARG" >&2
        exit 1
        ;;
    esac
  done
}

sub_help() {
    echo "Usage: $progName <subcommand> -e ENVIRONMENT [-c COMPONENT]"
    echo "Subcommands:"
    echo "    clean     Clean the compile folder (under _build)"
    echo "    compile   Use gomplate to compile the templates"
    echo "    validate  Validate the compiled templates against a Kubernetes API server"
    echo "    join      DEPRECATED: this is the same as compile now"
    echo "    deploy    Apply the compiled templates to a Kubernetes API server"
    echo "    delete    Delete the items in the compiled templates on a Kubernetes API server"
    echo ""
}

sub_clean() {
  echo "Cleaning _build folder..."
  rm -rf _build
}

sub_compile() {
  parse_args $@

  echo "Compiling for Environment: $env with Components: $componentBuildPath..."

  mkdir -p _build/$env/$componentBuildPath/templates
  gomplate --output-dir _build/$env/$componentBuildPath/templates --input-dir templates/$componentTemplatePath --datasource config=envs/$env.yaml
}

sub_join() {
  sub_compile $@
}

sub_validate() {
  sub_compile $@
  echo "Validating for $env..."
  kubectl apply -R --validate --dry-run -f _build/$env/$componentBuildPath/templates/
}

sub_deploy() {
  sub_compile $@
  echo "Deploying for $env..."
  kubectl apply -R -f _build/$env/$componentBuildPath/templates/
}

sub_delete() {
  sub_compile $@
  echo "Deleting items for $env with components $componentBuildPath..."
  kubectl delete -R -f _build/$env/$componentBuildPath/templates/
}

subcommand=$1
case $subcommand in
    "" | "-h" | "--help")
        sub_help
        ;;
    *)
        shift
        sub_${subcommand} $@
        if [ $? = 127 ]; then
            echo "Error: '$subcommand' is not a known subcommand." >&2
            echo "       Run '$progName --help' for a list of known subcommands." >&2
            exit 1
        fi
        ;;
esac
