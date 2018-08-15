# play-with-verts

The code we have been writing on the stream.

I've added a few comments so it's not exactly the same, but close enough.

## Episode-52 specifics

- Download the sponza files from http://www.crytek.com/cryengine/cryengine3/downloads
- Some files are missing so see here for details http://blog.duskzone.it/2013/04/01/of-missing-textures-assimp-the-crytek-sponza-atrium-and-naked-women/ do swap out with a more favorable texture but make sure you download the modified version of the `.mtl` file as those fixes are handy.
- update the `test2` functions in `assets.lisp` to points at your copy of the `sponza.obj` file.

## Getting Started

- `(ql:quickload :play-with-verts)`
- `(in-package :play-with-verts)`
- `(play :start)`

## Warning on Dependencies

If you are new to lisp I recommend waiting a few weeks until the next quicklisp release, then all the code you are seeing will be available through quicklisp and should just work.

## License

The license covers everything except the pngs, which are from http://seamless-pixels.blogspot.no/2012/09/free-seamless-ground-textures.html
