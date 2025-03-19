const fs = require("fs");
const showdown = require("showdown");
const Mustache = require("mustache");
const iconv = require("iconv-lite");

const mdConverter = new showdown.Converter();
const pageTemplate = fs.readFileSync("template.html", "utf8");

function generatePage(title, markdown, language, encoding="utf8")
{
    const pageContent = mdConverter.makeHtml(markdown);
    const data =
    {
        language: language,
        title: title,
        encoding: encoding,
        content: pageContent
    };
    return Mustache.render(pageTemplate, data);
}

const lang = "ru";

const functions =
[
    { title: "Engine", inFilename: `markdown/${lang}/functions/engine.md`, outFilename: `${lang}/html/functions/engine.html` },
    { title: "Pak", inFilename: `markdown/${lang}/functions/pak.md`, outFilename: `${lang}/html/functions/pak.html` },
    { title: "Viewer", inFilename: `markdown/${lang}/functions/viewer.md`, outFilename: `${lang}/html/functions/viewer.html` },
    { title: "PickList", inFilename: `markdown/${lang}/functions/picklist.md`, outFilename: `${lang}/html/functions/picklist.html` },
    { title: "Dummycube", inFilename: `markdown/${lang}/functions/dummycube.md`, outFilename: `${lang}/html/functions/dummycube.html` },
    { title: "Camera", inFilename: `markdown/${lang}/functions/camera.md`, outFilename: `${lang}/html/functions/camera.html` },
    { title: "Light", inFilename: `markdown/${lang}/functions/light.md`, outFilename: `${lang}/html/functions/light.html` },
    { title: "Font and Text", inFilename: `markdown/${lang}/functions/fonttext.md`, outFilename: `${lang}/html/functions/fonttext.html` },
    { title: "Sprite", inFilename: `markdown/${lang}/functions/sprite.md`, outFilename: `${lang}/html/functions/sprite.html` },
    { title: "HUDShape", inFilename: `markdown/${lang}/functions/hudshape.md`, outFilename: `${lang}/html/functions/hudshape.html` }
];

const outputEncoding = "win1251";
const metaEncoding = "windows-1251";

for (const f of functions)
{
    const markdown = fs.readFileSync(f.inFilename, "utf8");
    const output = generatePage(f.title, markdown, lang, metaEncoding);
    const encodedData = iconv.encode(output, outputEncoding);
    fs.writeFileSync(f.outFilename, encodedData);
}
