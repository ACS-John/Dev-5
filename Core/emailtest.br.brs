! this tests the email fucntionality
! this uses fnSendeemail to send an email
library "S:\core\fileio\fileio":fnSendEmail
library : fnInstallSendEmail
let fnInstallSendEmail

let success=fnSendEmail("laurajoysmith@gmail.com","This is a test message. Great.","test from ACS","")
pr success
pause

def library fnInstallSendEmail
   if ~exists("sendemail.exe") then
      ! It needs to be installed.
      execute "copy s:\core\sendemail.exe ."
   end if
fnend