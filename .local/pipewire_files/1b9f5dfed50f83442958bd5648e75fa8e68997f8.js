define("discourse/pre-initializers/theme-8-translations", ["exports"], function (_exports) {
  "use strict";

  Object.defineProperty(_exports, "__esModule", {
    value: true
  });
  _exports.default = void 0;
  var _default = {
    name: "theme-8-translations",
    initialize: function initialize() {
      /* Translation data for theme 8 (en)*/
      var data = {
        "en": {
          "search_banner": {
            "headline": "Welcome to our Forum",
            "subhead": "Please use the search function of the forum before posting any help requests."
          }
        }
      };

      for (var lang in data) {
        var cursor = I18n.translations;

        for (var _i = 0, _arr = [lang, "js", "theme_translations"]; _i < _arr.length; _i++) {
          var key = _arr[_i];
          cursor = cursor[key] = cursor[key] || {};
        }

        cursor[8] = data[lang];
      }
    }
  };
  _exports.default = _default;
});