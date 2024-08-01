/// @description Take screenshot
var x3dHomePath = environment_get_variable("userprofile") + "/Documents/Xtreme3D/screenshots";
if (not directory_exists(x3dHomePath))
{
	directory_create(x3dHomePath);
}

var screenshotNumber = 0;
var scrFilename;
do 
{
	scrFilename = x3dHomePath + "/screenshot_" + string(screenshotNumber) + ".png";
	screenshotNumber += 1;
}
until (not file_exists(scrFilename));

if (string_length(scrFilename) > 0)
	ViewerRenderToFile(viewer, scrFilename);
