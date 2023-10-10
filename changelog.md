# 2.0.0

Makes it easier to access the memory shared by each page of the SPA. 🎉

- Change `ApplicationProps`.
- Rename `Tepa.customPortStream` to `Tepa.portStream`
- Clean up DEPRECATED functions
- Add helper functions for "Linked Memory Pattern"
- Bugfix about _cancel_ port request
- Bugfix about `Tepa.Time.tick`

# 1.5.0

- Add `Tepa.mapLayer`
- Add `Tepa.Stream.break`, `Tepa.Stream.continue`, and `Tepa.Stream.customCase`
- Make `Tepa.inCase` deprecated

# 1.4.0

Add `Tepa.assertionError` and `Tepa.onEachLayer`.

# 1.3.0

Add `Tepa.bindAndThen2`, `Tepa.bindAndThen3`, and `Tepa.bindAndThenAll`.

# 1.2.1

Bugfix about view events. It was unintentionally resolved with any key.

# 1.2.0

- Add `Tepa.currentLayerId`
- Add `oneOf`, `inCase`, `Case` to `Tepa.Stream`

# 1.1.0

- Add `Tepa.customPortStream`
