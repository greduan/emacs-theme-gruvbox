# Gruvbox theme for Emacs

Gruvbox is a retro groove color scheme for Emacs. This is a port of the Vim
version originally by [Pavel Pertsev](https://github.com/morhetz) found
[here](https://github.com/morhetz/gruvbox).


## Installation and usage

1. Download `gruvbox-theme.el`, and put it in `~/.emacs.d/themes`. For example:
   ```shell
   curl https://raw.github.com/Greduan/emacs-theme-gruvbox/master/gruvbox-theme.el > ~/.emacs.d/themes/gruvbox-theme.el
   ```

1. Tell Emacs where to find themes in your `init.el` or `.emacs` file:
   ```lisp
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
   ```

1. Enable the theme:
   ```
   M-x load-theme RET gruvbox
   ```
   Or add the following to your `init.el` or `.emacs` file to load the theme at
   startup:
   ```
   (load-theme 'gruvbox t)
   ```

## Known bugs

None. For now...


## To-do

Add support for:

- [Rainbow Delimiters](http://www.emacswiki.org/emacs/RainbowDelimiters).

If you want for the theme to support something please open a new issue and I'll try my best to make it work out. :smile:


## License

Dual licensed under the MIT and GPLv3 licenses.


### MIT

```
The MIT License (MIT)

Copyright (c) 2013 Lee Machin

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```


### GPLv3

```
Gruvbox is a retro groove color scheme for Emacs. Port of the Vim version.
Copyright (C) 2013  Eduán Lávaque

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```
