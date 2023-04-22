(function () {
  var v = 'c234471f7e45510b2b0014cc10ab5826';
  var h1 = 'e2077d878327026c3cc4e35a6e7037d7';

  var p = CryptoJS.enc.Base64.parse("cDRyNG0zNzNy").toString(CryptoJS.enc.Latin1);
  var h2 = CryptoJS.MD5(v + h1).toString(CryptoJS.enc.Hex);

  console.log('p:', p);
  console.log('h2:', h2);
})();
