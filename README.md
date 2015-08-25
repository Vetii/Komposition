# Komposition

Komposition is based on the representation of a image as a 2D function used in the *Clastic* library.

Therefore, an image is a function `Point -> a`

An `Komposition` is also an instance of several usual classes.

## `Komposition` as a functor

`fmap` for images apply a function to the whole domain independently.

## `Komposition` as an applicative functor

With the `Applicative` instance for images, it is possible to combine two images,
in the same manner one combines them using blendings in “usual” image processing
programs.

## `Komposition` as a monad

This one is a bit trickier, but also a very new way to combine images.

Let’s consider the types of the `>>=` operator: `>>= :: m a -> (a -> m b ) -> m b`.

Which gives us, for images: `>>= :: Komposition a -> (a -> Komposition b) -> Komposition b`

The implementation of `>>=` reads the value of a pixel at a point, and
uses the signal to generate a new signal. A common way to use this is to transform
an image `b` using the value of an image `a`. Here, `a` plays the role of a displacement
map. 
