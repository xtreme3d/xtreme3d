/// @description Take screenshot
var screenshotsPath = environment_get_variable("userprofile") + "/Documents/Xtreme3D/screenshots";
if (not directory_exists(screenshotsPath))
{
	directory_create(screenshotsPath);
}

var screenshotNumber = 0;
var scrFilename;
do 
{
	scrFilename = screenshotsPath + "/screenshot_" + string(screenshotNumber) + ".png";
	screenshotNumber += 1;
}
until (not file_exists(scrFilename));

if (string_length(scrFilename) > 0)
	ViewerRenderToFile(viewer, scrFilename);
