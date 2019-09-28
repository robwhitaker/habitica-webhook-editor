Habitica Webhook Editor
=======================

A better webhook editor for Habitica.

---

## Usage

If you just want to use this project to edit some of your webhooks, there is a live version running [here](https://robwhitaker.com/habitica-webhook-editor/).

## Development

The easiest way to spin up your own version of the project is to install the [Nix package manager](https://nixos.org/nix/), which handles setting up a shell with the right versions of Elm and Elm-Format for the project. Once you have Nix installed, running the following commands will get you going with a development server.

```
$ git clone git@github.com:robwhitaker/habitica-webhook-editor.git # Clone the project to your local machine
$ cd habitica-webhook-editor # Move into the project directory
$ nix-shell # Start a shell with the dependencies installed
$ elm reactor # Start the development server
```

At this point, you should have a local version of the project running which you can access at `localhost:8000`.
