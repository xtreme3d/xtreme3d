const fs = require("fs");
const showdown = require("showdown");
const showdownHighlight = require("showdown-highlight");
const Mustache = require("mustache");
const iconv = require("iconv-lite");

const mdConverter = new showdown.Converter({
    extensions: [showdownHighlight({
        pre: true, // Whether to add the classes to the <pre> tag, default is false
        auto_detection: true // Whether to use hljs' auto language detection, default is true
    })]
});
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

const lang = "ru"; // TODO: make this command line argument

const functions =
[
    { title: "Constants", inFilename: `markdown/${lang}/functions/constants.md`, outFilename: `${lang}/html/functions/constants.html` },
    { title: "Engine", inFilename: `markdown/${lang}/functions/engine.md`, outFilename: `${lang}/html/functions/engine.html` },
    { title: "Pak", inFilename: `markdown/${lang}/functions/pak.md`, outFilename: `${lang}/html/functions/pak.html` },
    { title: "Viewer", inFilename: `markdown/${lang}/functions/viewer.md`, outFilename: `${lang}/html/functions/viewer.html` },
    { title: "PickList", inFilename: `markdown/${lang}/functions/picklist.md`, outFilename: `${lang}/html/functions/picklist.html` },
    { title: "Dummycube", inFilename: `markdown/${lang}/functions/dummycube.md`, outFilename: `${lang}/html/functions/dummycube.html` },
    { title: "Camera", inFilename: `markdown/${lang}/functions/camera.md`, outFilename: `${lang}/html/functions/camera.html` },
    { title: "Light", inFilename: `markdown/${lang}/functions/light.md`, outFilename: `${lang}/html/functions/light.html` },
    { title: "Font and Text", inFilename: `markdown/${lang}/functions/fonttext.md`, outFilename: `${lang}/html/functions/fonttext.html` },
    { title: "Sprite", inFilename: `markdown/${lang}/functions/sprite.md`, outFilename: `${lang}/html/functions/sprite.html` },
    { title: "HUDShape", inFilename: `markdown/${lang}/functions/hudshape.md`, outFilename: `${lang}/html/functions/hudshape.html` },
    { title: "Primitives", inFilename: `markdown/${lang}/functions/primitives.md`, outFilename: `${lang}/html/functions/primitives.html` },
    { title: "Actor", inFilename: `markdown/${lang}/functions/actor.md`, outFilename: `${lang}/html/functions/actor.html` },
    { title: "Freeform", inFilename: `markdown/${lang}/functions/freeform.md`, outFilename: `${lang}/html/functions/freeform.html` },
    { title: "Terrain", inFilename: `markdown/${lang}/functions/terrain.md`, outFilename: `${lang}/html/functions/terrain.html` },
    { title: "Object", inFilename: `markdown/${lang}/functions/object.md`, outFilename: `${lang}/html/functions/object.html` },
    { title: "Material", inFilename: `markdown/${lang}/functions/material.md`, outFilename: `${lang}/html/functions/material.html` },
    { title: "TextureEx", inFilename: `markdown/${lang}/functions/textureex.md`, outFilename: `${lang}/html/functions/textureex.html` },
    { title: "Shaders", inFilename: `markdown/${lang}/functions/shaders.md`, outFilename: `${lang}/html/functions/shaders.html` },
    { title: "ThorFX", inFilename: `markdown/${lang}/functions/thorfx.md`, outFilename: `${lang}/html/functions/thorfx.html` },
    { title: "FireFX", inFilename: `markdown/${lang}/functions/firefx.md`, outFilename: `${lang}/html/functions/firefx.html` },
    { title: "Lensflare", inFilename: `markdown/${lang}/functions/lensflare.md`, outFilename: `${lang}/html/functions/lensflare.html` },
    { title: "Skydome", inFilename: `markdown/${lang}/functions/skydome.md`, outFilename: `${lang}/html/functions/skydome.html` },
    { title: "Water", inFilename: `markdown/${lang}/functions/water.md`, outFilename: `${lang}/html/functions/water.html` },
    { title: "Blur", inFilename: `markdown/${lang}/functions/blur.md`, outFilename: `${lang}/html/functions/blur.html` },
    { title: "Skybox", inFilename: `markdown/${lang}/functions/skybox.md`, outFilename: `${lang}/html/functions/skybox.html` },
    { title: "Lines", inFilename: `markdown/${lang}/functions/lines.md`, outFilename: `${lang}/html/functions/lines.html` },
    { title: "Tree", inFilename: `markdown/${lang}/functions/tree.md`, outFilename: `${lang}/html/functions/tree.html` },
    { title: "Trail", inFilename: `markdown/${lang}/functions/trail.md`, outFilename: `${lang}/html/functions/trail.html` },
    { title: "Shadowplane", inFilename: `markdown/${lang}/functions/shadowplane.md`, outFilename: `${lang}/html/functions/shadowplane.html` },
    { title: "Shadowvolume", inFilename: `markdown/${lang}/functions/shadowvolume.md`, outFilename: `${lang}/html/functions/shadowvolume.html` },
    { title: "Mirror", inFilename: `markdown/${lang}/functions/mirror.md`, outFilename: `${lang}/html/functions/mirror.html` },
    { title: "Navigator", inFilename: `markdown/${lang}/functions/navigator.md`, outFilename: `${lang}/html/functions/navigator.html` },
    { title: "Movement", inFilename: `markdown/${lang}/functions/movement.md`, outFilename: `${lang}/html/functions/movement.html` },
    { title: "DCE", inFilename: `markdown/${lang}/functions/dce.md`, outFilename: `${lang}/html/functions/dce.html` },
    { title: "FPS", inFilename: `markdown/${lang}/functions/fps.md`, outFilename: `${lang}/html/functions/fps.html` },
    { title: "Partition", inFilename: `markdown/${lang}/functions/partition.md`, outFilename: `${lang}/html/functions/partition.html` },
    
    { title: "Tutorial 1. Basics", inFilename: `markdown/${lang}/tutorials/basics.md`, outFilename: `${lang}/html/tutorials/basics.html` },
    { title: "Tutorial 2. Simple scene", inFilename: `markdown/${lang}/tutorials/simplescene.md`, outFilename: `${lang}/html/tutorials/simplescene.html` },
    { title: "Tutorial 3. Object hierarchy", inFilename: `markdown/${lang}/tutorials/hierarchy.md`, outFilename: `${lang}/html/tutorials/hierarchy.html` }
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
