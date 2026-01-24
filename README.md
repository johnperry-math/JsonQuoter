# JsonQuoter
An Ada library to read a json-formated database of quotations.

This is similar to the Quoter repository, but that one was an executable
while this one is reworked as a library.

## Dependencies

This depends on [Jula](https://github.com/johnperry-math/jula).

## Building

To build, download [Jula](https://github.com/johnperry-math/jula) somewhere
(link above) then add it to your `GPR_PROJECT_PATH`. For example, I have
```bash
export GPR_PROJECT_PATH=../jula/
```

## License

CC-BY-SA-3.0.
This used to rely on GNATColl.JSON
but now relies on [Jula](https://github.com/johnperry-math/jula),
which better addresses my needs.
