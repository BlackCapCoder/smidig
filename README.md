# Agile projects

## Git

To contribute to the project you need to have `git` installed.

### Arch Linux

```bash
pacman -S git
```

### OSX

You should already have git installed, however, you might get an error that looks like this

```
xcrun: error: invalid active developer path (/Library/Developer/CommandLineTools), missing xcrun at: /Library/Developer/CommandLineTools/usr/bin/xcrun
```

You can fix this by running
```bash
xcode-select --install
```

Which is going to open a graphical wizard.


-----


To clone this project, run
```bash
git clone https://github.com/BlackCapCoder/smidig
```

You only need to do this once, next time you can just run `git clone` from within the `smidig` folder to download the latest version.


To upload your changes you need to make a commit:
```bash
git commit -m "message"
```

Replace `"message"` with what you have done- say "added event page" for instance.


Finally, to upload your commit
```bash
git push
```


## The backend

The backend uses the `stack` build tool, so you need this in order to compile it.

To install stack, run
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

To compile the backend, from within the `backend` folder, run
```bash
stack build
```

This is going to take a while the first time you do it, but should be fairly quick afterwards.

To run the backend
```bash
stack exec backend-exe
```

The website should now be available at [localhost:3000/login.html](http://localhost:3000/login.html)


## The frontend

The frontend consist of `html`, `css`, and `javascript` files located in the frontend folder. These files are served as is by the backend, except that the html files are automatically spliced into `theme.html`.


The backend also serves a magical file [localhost:3000/api.js](http://localhost:3000/api.js). This file is automatically generated by the server, and contains javascript code for all the API calls you can make (for instance, it contains a `getEvents` function that returns an array of all events).
