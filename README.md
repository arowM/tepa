# TEPA

[![test](https://github.com/arowM/tepa/actions/workflows/test.yaml/badge.svg)](https://github.com/arowM/tepa/actions/workflows/test.yaml)  
[Document](https://package.elm-lang.org/packages/arowM/tepa/latest/)  

![logo](https://user-images.githubusercontent.com/1481749/115139779-de382400-a06e-11eb-80e7-22af97774bfa.jpg)

TEPA (pronounced /tíːpə/) is a framework for building robust, user-centric Web applications.

## No exceptions

Exceptions are evil inventions that impede the development and prosperity of humanity. We painfully understand that you certainly do not want to think about the unexpected. You must want to put it off. But it is you who will get into trouble later.
Suppose you are building an airplane. Wouldn't you carefully inspect every single screw and replace it right away if it was defective? If the airplane fell down after it was all assembled, you would never know what actually caused the problem. Exception handling is such a bad technique.

Another evil of exceptions is that they mess with static types. Even though TypeScript introduces static types to JavaScript, you still have to deal with values of `unknown` types that you have no way of knowing what they are at compile time.

TEPA uses an exception-free language called Elm. With Elm, you are forced by the compiler to handle all unexpected things on the fly. It may seem rigid and depressing at first. But one day you will be grateful for this nagging compiler.

## No gap between scenario and implementation

Web applications should be user-centric, not developer-centric. If an application is not easy to use, users will leave. Without users, you cannot continue to develop applications. Eventually, your company will go out of business and you will lose your job.
So, no matter what kind of application you are developing, you must consider your users' point of view. Of course, if your company is spending its resources on creating a deeply brainwashed customer base, and no one complains that the application is impossibly difficult to use, then you can create an application that ignores the user in order to reduce development costs. However, it does not make sense to ignore the user and give priority to the convenience of the developer. It is necessary to think about the users and how an appropriate application should be.

An essential part of creating such a user-friendly application is the *user scenario*. The user scenario is a scenario of how a particular user would feel and operate the application, and what kind of feedback the application should provide in response to that operation.
Sometimes this is clearly written down and shared with the team, and sometimes it happens subconsciously in the designer's brain. Either way, it is important to be able to describe the behavior of the application in a way that closely resembles this scenario. Otherwise, "translation" work will be required to fill the gap between the two, and many bugs will be introduced in the process.
But for some reason, people pretend to be smart by using methods such as data-centric descriptions that put components in the lead role, or Reducer, which is stateless and eliminates even the context of a series of temporal operations, and so on, and instead increase the number of bugs caused by "translation". I don't know why they do so because I, Sakura-chan, the developer of TEPA, am a goat🐐

TEPA allows you to develop applications in a scenario-like format by writing along a timeline.

## Automatically test your scenarios.

As mentioned earlier, scenario creation is essential for user-centric applications, so TEPA provides a scenario creation feature. This feature allows you to create scenarios programmatically. The scenario can then be displayed in a browser in a nice format or output as a Markdown file.

Even more amazing is that the scenario can be turned directly into a lightweight test: write an application and a scenario in TEPA, and you can test whether it behaves as described in the scenario by emulating the behavior of the application. This testing is instantaneous because it is not done using a real web browser. For example, if you have a process that sleeps for 500 million years, you don't have to go into a cold sleep betting that the Earth will still be around in 500 million years. There is also no need to prepare mocks. Even for processes that depend on external conditions at runtime, such as requests to back-end servers or random number generation within the application, you can write the expected behavior in a scenario, and the automatically generated test will pass the results to the application for further testing.
Of course, being an emulator has its drawbacks. It does not actually render the screen, so visual regression testing is not possible. For the important parts, you should use something like Playwright or something similar.

## How to get started

Although tutorials for TEPA are currently under construction, a sample application is already available. Please see the contents of the [spa-sample directory](https://github.com/arowM/tepa/tree/main/spa-sample).

The [Elm language specification](https://guide.elm-lang.jp/core_language.html) is very small and easy to learn, and with the help of type annotations, reading TEPA's [module documentation](https://package.elm-lang.org/packages/arowM/tepa/latest/Tepa) makes it easy to understand `spa-sample` codes.

From the horizontal eyes of Sakura-chan, the goat, it seems that humans are creatures desperate to gain an advantage over others. Don't wait for the tutorial to be prepared, master TEPA before other humans do!
