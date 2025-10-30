# A Cloud API Service in Haskell

This project is meant to show how we deploy a simple haskell web api backend in AWS.
We'll use this project to discuss Haskell's functional ideas as we build out this project.

### Local Setup

I started this project from scratch by following these steps in my terminal.

Install `ghcup`:

```console
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Then to create the project:

```console
mkdir hs-user-mgr
cabal init -n --exe
cabal build
cabal run hs-user-mgr
cabal install ghcid
```

Create a local `.ghcid` file with the following contents:

```console
--command="cabal repl" --restart=hs-user-mgr.cabal --test Main.hs
```

Then start `ghcid` from within your project:

```console
> ghcid
```

To continually build your project as you develop it. The `.ghcid` file will help VSCode show the same errors `ghcid` finds while recompiling your code.

### Run Presentation

To start the presentation using `Patat` run the following commands in your terminal.

```console
> cabal install --lib patat
> cabal run patat -- ./presentation/boulder-haskell-meetup-2025.md
```

### Terraforming AWS resources

```console
> cd infrastructre
> terraform init
> terraform plan
> terraform apply
```

To clean up run:

```console
# to clean up any ECR images
> aws ecr batch-delete-image \
    --repository-name <repo-name> \
    --image-ids "$(aws ecr list-images --repository-name <repo-name> --query 'imageIds[*]' --output json)" \
    --region "us-east-1"
> terraform destroy
```
