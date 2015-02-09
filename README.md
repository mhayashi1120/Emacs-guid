guid.el
=======

guid.el provides Simple GUID/UUID generator/updator.

## Install:

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

```
(require 'guid)
```

## Usage:

* This guid package currently support guid and only uuid version 4
  defined at rfc4122 Section 4.4

* `guid-update-*' function is not simply replace GUID.
  These function keep identity if duplicated GUID exists
  while a one command.

* Update all GUID string in selected buffer.

```
M-x guid-update-buffer
```

* Update all GUID string in selected directory recursively.

```
M-x guid-update-directory
```

* Update all GUID string in selected file.

```
M-x guid-update-file
```

* Generate GUID string

```
M-x guid-generate-string
```

=> Now `kill-ring` has generated GUID.

