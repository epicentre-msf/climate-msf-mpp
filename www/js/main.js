function isMobileDevice() {
  var userAgent = navigator.userAgent || navigator.vendor || window.opera;

  // Regular expression patterns to identify mobile devices
  var mobileDevicePatterns = [
    /Android/i,
    /webOS/i,
    /iPhone/i,
    /iPad/i,
    /iPod/i,
    /BlackBerry/i,
    /Windows Phone/i,
    /Opera Mini/i,
    /IEMobile/i,
    /Mobile/i
  ];

  // Check the user agent string against the patterns
  for (var i = 0; i < mobileDevicePatterns.length; i++) {
    if (mobileDevicePatterns[i].test(userAgent)) {
      return true;
    }
  }
  return false;
}

$(document).on('shiny:sessioninitialized', function(event) {
  var isMobile = isMobileDevice();
  var userAgent = navigator.userAgent || navigator.vendor || window.opera;
  Shiny.setInputValue('is_mobile', isMobile)
  Shiny.setInputValue('user_agent', userAgent)
})
