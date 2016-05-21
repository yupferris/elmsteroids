# elmsteroids
A non-trivial Asteroids clone written in [Elm](http://elm-lang.org/) 0.17.

![screen shot 2016-05-20 at 7 33 13 pm](https://cloud.githubusercontent.com/assets/3166056/15445923/c84e6c78-1ec1-11e6-9b6b-514e0871bfc4.png)

## Live Demo
[Play elmsteroids here!](http://yupferris.github.io/elmsteroids/)

## Controls
<kbd>←</kbd>, <kbd>↑</kbd>, <kbd>→</kbd> and <kbd>↓</kbd> move, <kbd>space</kbd> shoots. <kbd>enter/return</kbd> moves to the next game state (where applicable). That's it!

## Details/Post Mortem
This is an Asteroids clone written in Elm. It was written to get a feel for Elm with a somewhat non-trivial project, which is why I went nuts with details like precise collisions, proper space wrapping, particles, various game states, etc. Overall, it was a fun project, took me about a week's worth of evenings to do, and was especially fun to implement in a purely functional language.

Elm in particular is something I've had my eye on for a couple years, but never sat down to give it a shot. What I found attractive seeing it before was that it looked a lot like Haskell and used FRP to update the UI. Recently, however, that story changed a bit. Elm put out an article called [A Farewell to FRP](http://elm-lang.org/blog/farewell-to-frp), where a new FRP-free architecture was outlined, and that Elm 0.17 and onwards would use this new approach. Most of the details didn't make any sense to be because I had never used the language before, so I thought it was about time to give it a go.

I've done a lot of stateless things before, but never tried making a stateless games, and it looked like Elm was a natural fit. I wanted to do a project where I could get away with minimal graphics but still make things look and feel polished, so an Asteroids clone was a very natural choice.

Most of the implementation was lovely. It's been a bit since I've done something purely functional, and that "happy feeling" statelessness gives me was certainly present. The overall language felt very familiar having done a little bit of Haskell and a lot of F# previously. [elm-mode](https://github.com/jcollard/elm-mode) definitely did the trick (minus some painful indentation idiosynchrosies), and the reactor provided a nice way of working. Generally, it just felt nice working on this :)

However, there were definitely a few pain points. For one thing, this was written right after 0.17 was publicly released, and not all of the documentation etc has caught up. At the time of writing, large portions of it are unfinished, with notes like "this guide is coming soon, but it works how it used to", which is particularly unhelpful for someone like me who hasn't used Elm before. I spent much more time than I'd like to admit reading through the standard library source code and wading through random 3rd-party github repo's because of this. This also meant no time travelling debugger, which is a bit of a disappointment, since that was supposed to be one of Elm's flagship features previously.

Another major source of pain is Elm's inexpressiveness and lack of standard library support for common things you'd expect in a purely-functional language. For example, Elm doesn't support higher-kinded types, which means no common classes like `Functor` or `Monad`, although you _can_ describe instances of them, just without the ability to abstract over them. Consequences of this are no `do`-notation (see [Asteroids.elm](https://github.com/yupferris/elmsteroids/blob/master/src/Asteroids.elm#L112)), no list comprehensions (see [Collisions.elm](https://github.com/yupferris/elmsteroids/blob/master/src/Collisions.elm#L76)), and a huge void where helpful functions like `liftA2` ought to be. This means if you want to do anything beyond trivial code, you're going to be implementing these yourself (see [State.elm](https://github.com/yupferris/elmsteroids/blob/master/src/State.elm)). This isn't usually so bad for isolated cases, but to cover all the bases for each project and library the community makes means everyone's going to have their own versions of these floating around, when the batteries probably should've been included.

I think Elm is an interesting language/toolchain. It's a fairly minimal language that sits somewhere between Haskell and F#, but feels to me like it really lacks the power of either its counterparts. On the purely functional side, it lacks many of the out-of-the-box tools you'd expect to help you solve sizeable problems and do things properly in a functional manner without making a mess. On the accessibility side, the syntax is fairly light and the standard library is by no means overwhelming, but there's likely still going to be a large gap when transitioning from Elm to a more mature language/environment, and the compatibility story appears "fragmented" at best. On the web side, there are many other frameworks proven to solve the "let's build scalable stuff", and to my knowledge Elm very much lacks larger examples of doing this. I'm not saying one _can't_ build a large state-of-the-art web app, but I am saying "proceed with caution" if your plan is to do so _right now_.

All in all, this was definitely a fun project, and I'm glad I tried Elm. But I don't think it has the potential to become a "go-to" in my toolbox any time soon. Still, I recommend you give it a shot and make something cool with it. It definitely seems to fit the bill for making "cute little stateless things" like this :)

## License
This code is licensed under the BSD2 license (see LICENSE).
