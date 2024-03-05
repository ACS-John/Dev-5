# Sad Panda
Sad Panda is a compiler for Business Rules! programs that allows for source code without line numbers.  It also extends the syntax of the language and allow for customized plugins and a variety of other features.

## Setup Instructions

Next acquire a brserial.dat (license) file from Business Rules Corp.
A free personal use license is available here:
http://www.brulescorp.com/pricing.html#useLicense

Place this file in the Sad Panda program directory.

Notepad++
	To add Ctrl+Alt+1 to Compile add a line similar to the following to "shortcuts.xml" in you Notepad++ settings folder.
		<Command name="Sad Panda Compile" Ctrl="yes" Alt="yes" Shift="no" Key="49">&quot;C:\ACS\Dev-5\Sad Panda\Compile.cmd&quot; &quot;$(FULL_CURRENT_PATH)&quot;</Command>
