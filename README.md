# cl-atomic

A portable atomic library for Common Lisp. 

Note: `cl-atomic` currently support `atomic-fixnum` `atomic-boolean` for some Common Lisp implementations, see `compatibility` below

-----------------------------------------------------------------
## dependencies

none

-----------------------------------------------------------------
## install

```shell
git clone https://github.com/muyinliu/cl-atomic.git
cp -r cl-atomic ~/quicklisp/local-projects/
```

Note: will request adding cl-atomic to QuickLisp later

-----------------------------------------------------------------
## usage

### load with QuickLisp

```lisp
(ql:quickload 'cl-atomic)
```

### demos

```lisp
(let ((atomic-fixnum (atomic:make-atomic-fixnum :value 1024)))
  (atomic:atomic-fixnum-incf atomic-fixnum)
  (atomic:atomic-fixnum-value atomic-fixnum))
```
=>
```=>
1025
```

```lisp
(let ((atomic-boolean (atomic:make-atomic-boolean :value nil)))
  (atomic:atomic-boolean-compare-and-swap atomic-boolean nil t)
  (atomic:atomic-boolean-value atomic-boolean))
```
=>
```=>
T
```

Note: more demos see directory `test` in `cl-atomic`

### `atomic:atomic-fixnum`

#### compatibility

| implementation | support-p | note                             |
|----------------|-----------|----------------------------------|
| SBCL           | √         |                                  |
| CCL            | √         |                                  |
| CMUCL          | √         |                                  |
| ABCL           | ×         | ABCL NOT support CAS             |
| ECL            | √         |                                  |
| CLISP          | ×         | CLISP NOT support CAS            |
| LispWorks 6+   | √*        |                                  |
| AllegroCL      | √*        | only support **non-SMP** version |

Note: LispWorks NOTE: only test on LispWorks 6 32-bit for macOS Personal Edition and LispWorks 7 64-bit for macOS Personal Edition

Note: AllegroCL NOTE: only test on AllegroCL 10.1 32-bit Free Express for macOS, never test on AllegroCL commercial version because no license.

#### functions/macros

##### `atomic:make-atomic-fixnum &key value`, create `atomic-fixnum`

##### `atomic:atomic-fixnum-value atomic-fixnum`, get value of `atomic-fixnum`

##### `atomic:atomic-fixnum-incf atomic-fixnum &optional diff`, atomic incf `atomic-fixnum` and return old value

Be careful! Calling `atomic:atomic-fixnum-incf` or `atomic:atomic-fixnum-incf-and-get` of `atomic-fixnum` with value `most-positive-fixnum` will return `most-negative-fixnum`:

```lisp
(let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-positive-fixnum)))
  (atomic:atomic-fixnum-incf atomic-fixnum)
  (= (atomic:atomic-fixnum-value atomic-fixnum)
     most-negative-fixnum))
```
=>
```=>
T
```

##### `atomic:atomic-fixnum-incf-and-get fixnum &optional diff`, atomic incf `aotmic-fixnum` and return new value

##### `atomic:atomic-fixnum-decf atomic-fixnum &optional diff`, atomic decf `atomic-fixnum` and return old value

Be careful! Calling `atomic:atomic-fixnum-decf` or `atomic:atomic-fixnum-decf-and-get` of `atomic-fixnum` with value `most-negative-fixnum` will return `most-positive-fixnum`:

```lisp
(let ((atomic-fixnum (atomic:make-atomic-fixnum :value most-negative-fixnum)))
  (atomic:atomic-fixnum-decf atomic-fixnum)
  (= (atomic:atomic-fixnum-value atomic-fixnum)
     most-positive-fixnum))
```
=>
```=>
T
```

##### `atomic:atomic-fixnum-decf-and-get atomic-fixnum &optional diff`, atomic decf `atomic-fixnum` and return new value

##### `atomic:atomic-fixnum-compare-and-swap atomic-fixnum old new`, atomic CAS `atomic-fixnum`

Note: return `T` if success otherwise return `NIL`

##### `atomic:atomic-fixnum-swap atomic-fixnum old new`, atomic swap `atomic-fixnum` and return new value

##### `atomic:atomic-fixnum-get-and-set atomic-fixnum new`, atomic swap `atomic-fixnum` and return old value

#### constants

##### `atomic:+atomic-fixnum-max+`, same as `most-positive-fixnum`

| implementation | N-bit | `atomic:+atomic-fixnum-max+` | note   |
|----------------|------:|-----------------------------:|--------|
| SBCL           |    64 |          4611686018427387903 | 2^62-1 |
| SBCL           |    32 |                   1073741823 | 2^30-1 |
| CCL            |    64 |          1152921504606846975 | 2^60-1 |
| CMUCL          |    32 |                    536870911 | 2^29-1 |
| ECL            |    64 |          2305843009213693951 | 2^61-1 |
| LispWorks 6+   |    64 |          1152921504606846975 | 2^60-1 |
| LispWorks 6+   |    32 |                    536870911 | 2^29-1 |
| AllegroCL      |    32 |                    536870911 | 2^29-1 |

##### `atomic:+atomic-fixnum-min+`, same as `most-negative-fixnum`

| implementation | N-bit | `atomic:+atomic-fixnum-min+` | note  |
|----------------|------:|-----------------------------:|-------|
| SBCL           |    64 |         -4611686018427387904 | -2^62 |
| SBCL           |    32 |                  -1073741824 | -2^30 |
| CCL            |    64 |         -1152921504606846976 | -2^60 |
| CMUCL          |    32 |                   -536870912 | -2^29 |
| ECL            |    64 |         -2305843009213693952 | -2^61 |
| LispWorks 6+   |    64 |         -1152921504606846976 | -2^60 |
| LispWorks 6+   |    32 |                   -536870912 | -2^29 |
| AllegroCL      |    32 |                   -536870912 | -2^29 |

### `atomic:atomic-boolean`

#### compatibility

| implementation  | support-p | note                  |
|-----------------|-----------|-----------------------|
| SBCL            | √         |                       |
| CCL             | √         |                       |
| CMUCL           | √         |                       |
| ABCL            | ×         | ABCL NOT support CAS  |
| ECL             | √         |                       |
| CLISP           | ×         | CLISP NOT support CAS |
| LispWorks 6+    | √         |                       |
| AllegroCL       | √         |                       |

#### functions/macros

##### `atomic:make-atomic-boolean &key value`, create `atomic-boolean`

##### `atomic:atomic-boolean-value atomic-boolean`, get value of `atomic-boolean`

##### `atomic:atomic-boolean-compare-and-swap atomic-boolean`, atomic CAS `atomic-boolean`

##### `atomic:atomic-boolean-swap atomic-boolean old new`, atomic swap `atomic-boolean` and return new value

##### `atomic:atomic-boolean-get-and-set atomic-boolean new`, atomic swap `atomic-fixnum` and return old value

-----------------------------------------------------------------
## test

Any reply is welcome!

Note: I don't have any commercial versions of LispWorks or AllegroCL. So if you own those license, please help running the test cases. Thanks!

### dependencies

- [prove](https://github.com/fukamachi/prove)
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads/)
- [cl-all](https://github.com/Shinmera/cl-all) optional

### with cl-all(recommended)

Recommended [cl-all](https://github.com/Shinmera/cl-all) for testing `cl-atomic` in multiple Common Lisp implementations:

```shell
cl-all sbcl ccl cmucl ecl allegro "(ql:quickload 'cl-atomic-test)"
```

### with SBCL

In command line:

```shell
sbcl --noinform --eval "(ql:quickload 'cl-atomic-test)" --quit
```

OR in REPL of SBCL:

```lisp
(ql:quickload 'cl-atomic-test)
```

### with LispWorks

#### with LispWorks Personal Edition

You have to run following lisp code in REPL of LispWorks IDE if you are using LispWorks Personal Edition because loading init file is forbidden:

```lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)))

(ql:quickload 'cl-atomic-test)
```

### with other implementations

```lisp
(ql:quickload 'cl-atomic-test)
```

Note: see [cl-all](https://github.com/Shinmera/cl-all) for more details

-----------------------------------------------------------------
## benchmark

You might have to append text line `/path/to/cl-atomic/cl-atomic-benchmark.asd` to `~/quicklisp/local-projects/systems-index.txt`

### dependencies

- [prove](https://github.com/fukamachi/prove)
- [bordeaux-threads](https://github.com/sionescu/bordeaux-threads/)
- [cl-all](https://github.com/Shinmera/cl-all) optional

### with cl-all(recommended)

Recommended [cl-all](https://github.com/Shinmera/cl-all) for benchmark `cl-atomic` in multiple Common Lisp implementations:

```shell
cl-all sbcl ccl cmucl ecl allegro "(ql:quickload 'cl-atomic-benchmark)"
```

### with SBCL

In command line:

```shell
sbcl --noinform --eval "(ql:quickload 'cl-atomic-benchmark)" --quit
```

OR in REPL of SBCL:

```lisp
(ql:quickload 'cl-atomic-benchmark)
```

### benchmark result

#### benchmark result of `atomic:atomic-fixnum-incf` of `atomic-fixnum`

On my MacBook Pro mid2014(with CPU Intel® Core™ i7-4770HQ 3.4 GHz, with macOS 10.13.6):

| implementation             | version |  avg op/s |  max op/s |  min op/s | note            |
|----------------------------|--------:|----------:|----------:|----------:|-----------------|
| SBCL                       |  1.3.21 |  43668122 |  48309178 |  41067761 |                 |
| CCL                        |  1.11.5 |  16891891 |  17769595 |  16161328 | DarwinX8664     |
| CMUCL                      |     21d | 135135135 | 142857142 | 133333333 | non-SMP         |
| ECL                        | 20.4.24 |  28735632 |  30487804 |  28089887 |                 |
| Allegro CL Free Expression |    10.1 |  67114093 |  67796610 |  66666666 | non-SMP version |
| LispWorks Personal Edition |   6.1.2 |  36832412 |  37735849 |  35971223 | 32-bit          |
| LispWorks Personal Edition |   7.1.2 |  43478260 |  49019607 |  39603960 | 64-bit          |

-----------------------------------------------------------------
## TODO

- Fall back to use locks for Common Lisp implementations NOT support compare-and-swap/atomic-incf/atomic-decf/...

-----------------------------------------------------------------
## license

MIT(see LICENSE file for details).
