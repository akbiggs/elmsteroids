# elmsteroids
An Asteroids clone written in [Elm](http://elm-lang.org/) 0.17.

![screen shot 2016-05-20 at 7 33 13 pm](https://cloud.githubusercontent.com/assets/3166056/15445923/c84e6c78-1ec1-11e6-9b6b-514e0871bfc4.png)

## Live Demo
[Play elmsteroids here!](http://yupferris.github.io/elmsteroids/)

## Controls
Arrow keys move, space shoots. Enter/return moves to the next game state (where applicable). That's it!

## Details/Post Mortem
This is an Asteroids clone written in Elm. It was written to get a feel for Elm with a somewhat non-trivial project, which is why I went nuts with details like precise collisions, proper space wrapping, particles, various game states, etc. Overall, it was a fun project, and it was especially fun to implement in a purely functional language.

However, there were definitely a few pain points. For one thing, this was written right after 0.17 was publicly released, and not all of the documentation etc has caught up. At the time of writing, large portions of it are unfinished, with notes like "this guide is coming soon, but it works how it used to", which is particularly unhelpful for someone like me who hasn't used Elm before. I spent much more time than I'd like to admit reading through the standard library source code and wading through random 3rd-party github repo's because of this. This also meant no time travelling debugger, which is a bit of a disappointment, since that was supposed to be one of Elm's flagship features previously.

Another major source of pain is Elm's inexpressiveness and lack of standard library support for common things you'd expect in a purely-functional language. For example, Elm doesn't support higher-kinded types, which means no common classes like `Functor` or `Monad`, although you _can_ describe instances of them, just without the ability to abstract over them. Consequences of this are no `do`-notation (see [Asteroids.elm](https://github.com/yupferris/elmsteroids/blob/master/src/Asteroids.elm#L112)), no list comprehensions (see [Collisions.elm](https://github.com/yupferris/elmsteroids/blob/master/src/Collisions.elm#L76)), and a huge void where helpful functions like `liftA2` ought to be. This means if you want to do anything beyond trivial code, you're going to be implementing these yourself (see [State.elm](https://github.com/yupferris/elmsteroids/blob/master/src/State.elm)). This isn't usually so bad for isolated cases, but to cover all the bases for each project and library the community makes means everyone's going to have their own versions of these floating around, when the batteries probably should've been included.

I think Elm is an interesting language/toolchain. It's a fairly minimal language that sits somewhere between Haskell and F#, but feels to me like it really lacks the power of either its counterparts. On the purely functional side, it lacks many of the out-of-the-box tools you'd expect to help you solve sizeable problems and do things properly in a functional manner without making a mess. On the accessibility side, the syntax is fairly light and the standard library is by no means overwhelming, but there's likely still going to be a large gap when transitioning from Elm to a more mature language/environment, and the compatibility story appears "fragmented" at best. On the web side, there are many other frameworks proven to solve the "let's build scalable stuff", and to my knowledge Elm very much lacks larger examples of doing this. I'm not saying one _can't_ build a large state-of-the-art web app, but I am saying "proceed with caution" if your plan is to do so _right now_.

All in all, this was definitely a fun project, and I'm glad I tried Elm. But I don't think it has the potential to become a "go-to" in my toolbox any time soon. Still, I recommend you give it a shot and make something cool with it. It definitely seems to fit the bill for making "cute little stateless things" like this :)

## License
This code is licensed under the BSD2 license (see LICENSE).
