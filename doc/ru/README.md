Xtreme3D Documentation (Russian)
================================
This HTML documentation is meant to be compiled to CHM format. You can use any available tool for that (we recommend our [CHM Builder](https://github.com/xtreme3d/chm-builder)). Warning: text encoding of all files is Windows-1251, this is intentional because CHM Builder doesn't support UTF-8.

To compile the CHM file on Windows, `buildchm.bat` can be used. If you opened `help-ru.chm`, close it before calling the script to unlock the file, otherwise compilation will fail.

Documentation Generator
-----------------------
The efforts are currently being made to rewrite documentation in Markdown and autogenerate HTML pages. This is done with Node.js. Run `node buildhtml.js` to generate pages that already have Markdown sources.
