# kv

... is a simple CLI-accessible key value store written in _Chicken-Scheme_ and using _CSV_ as a backend.

While this sort of program may be useful for storing some commonly required data in a easily accessible fashion its primary purpose for me is to be used as a _Chicken-Scheme_ tryout _platform_.

## Example usage:

Listing all stores: `kv` or `kv show`

Listing all keys of store _test_: `kv test` or `kv show test`

Accessing value of key _example_ in store _test_: `kv test example` or `kv show test example`

Listing all keys and values of all stores: `kv all`

Listing all keys and values of store _test_: `kv all test`

Replacing or creating the value of key _example_ in store _test_ with _dummy_: `kv write test example dummy`

Deleting key _example_ in store _test_: `kv delete test example`

Renaming key _example_ in store _test_ to _example2_: `kv rename test example example2`
