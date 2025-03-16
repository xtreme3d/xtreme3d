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

const functions =
[
    { title: "Engine", inFilename: "markdown/functions/engine.md", outFilename: "html/functions/engine.html" },
    { title: "Pak", inFilename: "markdown/functions/pak.md", outFilename: "html/functions/pak.html" },
    { title: "Viewer", inFilename: "markdown/functions/viewer.md", outFilename: "html/functions/viewer.html" },
    { title: "PickList", inFilename: "markdown/functions/picklist.md", outFilename: "html/functions/picklist.html" },
    { title: "Dummycube", inFilename: "markdown/functions/dummycube.md", outFilename: "html/functions/dummycube.html" },
    { title: "Camera", inFilename: "markdown/functions/camera.md", outFilename: "html/functions/camera.html" },
    { title: "Light", inFilename: "markdown/functions/light.md", outFilename: "html/functions/light.html" }
];

const outputEncoding = "win1251";
const metaEncoding = "windows-1251";

for (const f of functions)
{
    const markdown = fs.readFileSync(f.inFilename, "utf8");
    const output = generatePage(f.title, markdown, "ru", metaEncoding);
    const encodedData = iconv.encode(output, outputEncoding);
    fs.writeFileSync(f.outFilename, encodedData);
}
