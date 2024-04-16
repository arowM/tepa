# 6.0.0

- Add `setKeyAndId` field to `ViewContext`
- Add `Tepa.pushKeyPrefix`

# 5.0.1

Trivial fix for README.

# 5.0.0

Drop support for TEA to get out tech geeks.

# 4.0.0

Simplify architecture, and support structures that has `Layer` in other `Layer`.

- Remove functions:
  - `Tepa.mapViewContext`
  - `Tepa.layerStateOf`
  - `Tepa.layerIdOf`
  - `Tapa.onEachLayer`
  - `Tepa.neverResolved`
  - `Tepa.mapLayer`
  - `Tepa.currentLayerId`
  - `Tepa.getFormState`
  - `Tepa.FormState`
- Add functions:
  - `Tepa.forEach`
  - `Tepa.onChildLayer`
  - `Tepa.Stream.indexedMap`
- Changed:
  - `layerId` and `state` field of `ViewContext` is removed.
  - Changed the callback for `Tepa.Stream.customCase` so that it does not receive current state.

# 3.0.0

Better support for the _Linked Memory Pattern_ at the architecture level.

- Remove `layerState`
  Use `layerStateOf` instead.
- Remove `isOnSameLayer`
  Alternatively, compare layer IDs by using `layerIdOf`.
- Change type for `onLayer` and `onEachLayer`
  Enforce Linked Memory Pattern to use Layer.
- Add `LayerMemory`, `onLink`, `onBody`, and following helper functions
  - `linkSequence`, `bodySequence`
  - `modifyLink`, `modifyBody`

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
