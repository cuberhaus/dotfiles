(function() {
  if ('Discourse' in window && Discourse.__container__) {
    Discourse.__container__
      .lookup("service:theme-settings")
      .registerSettings(8, {"show_on":"homepage","background_image":"none","tile_background_image":false,"show_for":"everyone"});
  }
})();
(function() {
  if ('Ember' in window) {
    Ember.TEMPLATES["/connectors/above-main-container/search-banner"] = Ember.HTMLBars.template({"id":null,"block":"{\"symbols\":[],\"statements\":[[0,\"\\n\\n\"],[4,\"if\",[[24,[\"show_for\"]]],null,{\"statements\":[[4,\"if\",[[24,[\"displaySearchBanner\"]]],null,{\"statements\":[[0,\"      \"],[7,\"div\",true],[10,\"class\",\"custom-search-banner\"],[8],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"wrap custom-search-banner-wrap\"],[8],[0,\"\\n          \"],[7,\"h1\",true],[8],[1,[28,\"theme-i18n\",[8,\"search_banner.headline\"],null],false],[9],[0,\"\\n          \"],[7,\"p\",true],[8],[1,[28,\"theme-i18n\",[8,\"search_banner.subhead\"],null],false],[9],[0,\"\\n          \"],[1,[28,\"mount-widget\",null,[[\"widget\"],[\"search-widget\"]]],false],[0,\"\\n        \"],[9],[0,\"\\n      \"],[9],[0,\"\\n\"]],\"parameters\":[]},null],[0,\"\\n\"]],\"parameters\":[]},null]],\"hasEval\":false}","meta":{}});
  }
})();
(function () {
  if ('Discourse' in window && typeof Discourse._registerPluginCode === 'function') {
    var __theme_name__ = "discourse-search-banner";

    var settings = Discourse.__container__.lookup("service:theme-settings").getObjectForTheme(8);

    var themePrefix = function themePrefix(key) {
      return "theme_translations.8.".concat(key);
    };

    Discourse._registerPluginCode('0.8', function (api) {
      try {
        api.registerConnectorClass('above-main-container', 'search-banner', {
          setupComponent: function setupComponent(args, component) {
            var topMenuRoutes = Discourse.SiteSettings.top_menu.split('|').map(function (route) {
              return '/' + route;
            });
            var homeRoute = topMenuRoutes[0];
            api.onPageChange(function (url, title) {
              var home = url === "/" || url.match(/^\/\?/) || url === homeRoute;

              if (settings.show_on === "homepage") {
                var showBannerHere = home;
              } else if (settings.show_on === "top_menu") {
                var showBannerHere = topMenuRoutes.indexOf(url) > -1 || home;
              } else {
                var showBannerHere = url.match(/.*/) && !url.match(/search.*/) && !url.match(/admin.*/);
              }

              if (showBannerHere) {
                component.set('displaySearchBanner', true);
                $('html').addClass('display-search-banner');
              } else {
                component.set('displaySearchBanner', false);
                $('html').removeClass('display-search-banner');
              }

              if (settings.show_for === "everyone") {
                component.set('show_for', true);
              } else if (settings.show_for === "logged_out" && !api.getCurrentUser()) {
                component.set('show_for', true);
              } else if (settings.show_for === "logged_in" && api.getCurrentUser()) {
                component.set('show_for', true);
              } else {
                component.set('show_for', false);
                $('html').removeClass('display-search-banner');
              }
            });
          }
        });
      } catch (err) {
        var rescue = require("discourse/lib/utilities").rescueThemeError;

        rescue(__theme_name__, err, api);
      }
    });
  }
})();function _toConsumableArray(arr) { return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread(); }

function _nonIterableSpread() { throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method."); }

function _unsupportedIterableToArray(o, minLen) { if (!o) return; if (typeof o === "string") return _arrayLikeToArray(o, minLen); var n = Object.prototype.toString.call(o).slice(8, -1); if (n === "Object" && o.constructor) n = o.constructor.name; if (n === "Map" || n === "Set") return Array.from(o); if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen); }

function _iterableToArray(iter) { if (typeof Symbol !== "undefined" && Symbol.iterator in Object(iter)) return Array.from(iter); }

function _arrayWithoutHoles(arr) { if (Array.isArray(arr)) return _arrayLikeToArray(arr); }

function _arrayLikeToArray(arr, len) { if (len == null || len > arr.length) len = arr.length; for (var i = 0, arr2 = new Array(len); i < len; i++) { arr2[i] = arr[i]; } return arr2; }

(function () {
  if ('Discourse' in window && typeof Discourse._registerPluginCode === 'function') {
    var __theme_name__ = "discourse-search-banner";

    var settings = Discourse.__container__.lookup("service:theme-settings").getObjectForTheme(8);

    var themePrefix = function themePrefix(key) {
      return "theme_translations.8.".concat(key);
    };

    Discourse._registerPluginCode('0.8', function (api) {
      try {
        // Simplified version of header search theme component
        var searchMenuWidget = api.container.factoryFor('widget:search-menu');
        var corePanelContents = searchMenuWidget.class.prototype['panelContents'];
        api.reopenWidget('search-menu', {
          buildKey: function buildKey(attrs) {
            var type = attrs.formFactor || 'menu';
            return "search-".concat(type);
          },
          defaultState: function defaultState(attrs) {
            return {
              formFactor: attrs.formFactor || 'menu',
              showHeaderResults: false
            };
          },
          html: function html() {
            var _this = this;

            if (this.state.formFactor === 'widget') {
              return this.panelContents();
            } else {
              return this.attach('menu-panel', {
                maxWidth: 500,
                contents: function contents() {
                  return _this.panelContents();
                }
              });
            }
          },
          clickOutside: function clickOutside() {
            if (!this.vnode.hooks['widget-mouse-down-outside']) {
              return this.mouseDownOutside();
            }
          },
          mouseDownOutside: function mouseDownOutside() {
            var formFactor = this.state.formFactor;

            if (formFactor === 'menu') {
              return this.sendWidgetAction('toggleSearchMenu');
            } else {
              this.state.showHeaderResults = false;
              this.scheduleRerender();
            }
          },
          click: function click() {
            var formFactor = this.state.formFactor;

            if (formFactor === 'widget') {
              this.showResults();
            }
          },
          showResults: function showResults() {
            this.state.showHeaderResults = true;
            this.scheduleRerender();
          },
          linkClickedEvent: function linkClickedEvent() {
            var formFactor = this.state.formFactor;

            if (formFactor === 'widget') {
              $('#search-term').val('');
              $('.search-placeholder').css('visibility', 'visible');
              this.state.showHeaderResults = false;
              this.scheduleRerender();
            }
          },
          panelContents: function panelContents() {
            var _contents;

            var formFactor = this.state.formFactor;
            var showHeaderResults = this.state.showHeaderResults == null || this.state.showHeaderResults === true;
            var contents = [];

            if (formFactor === 'widget') {
              contents.push(this.attach('button', {
                icon: 'search',
                className: 'search-icon',
                action: 'showResults'
              }));
            }

            contents = (_contents = contents).concat.apply(_contents, _toConsumableArray(corePanelContents.call(this)));
            var results = contents.find(function (w) {
              return w.name == 'search-menu-results';
            });

            if (results && results.attrs.results) {
              $('.search-menu.search-header').addClass('has-results');
            } else {
              $('.search-menu.search-header').removeClass('has-results');
            }

            if (formFactor === 'menu' || showHeaderResults) {
              return contents;
            } else {
              return contents.filter(function (widget) {
                return widget.name != 'search-menu-results' && widget.name != 'search-context';
              });
            }
          }
        });
        api.createWidget("search-widget", {
          tagName: "div.search-widget"
        });
        api.decorateWidget('search-widget:after', function (helper) {
          var searchWidget = helper.widget,
              appController = helper.register.lookup('controller:application'),
              searchMenuVisible = searchWidget.state.searchVisible;

          if (!searchMenuVisible && !searchWidget.attrs.topic) {
            return helper.attach('search-menu', {
              contextEnabled: searchWidget.state.contextEnabled,
              formFactor: 'widget'
            });
          }
        });
      } catch (err) {
        var rescue = require("discourse/lib/utilities").rescueThemeError;

        rescue(__theme_name__, err, api);
      }
    });
  }
})();(function () {
  if ('Discourse' in window && typeof Discourse._registerPluginCode === 'function') {
    var __theme_name__ = "discourse-search-banner";

    var settings = Discourse.__container__.lookup("service:theme-settings").getObjectForTheme(8);

    var themePrefix = function themePrefix(key) {
      return "theme_translations.8.".concat(key);
    };

    Discourse._registerPluginCode('0.1', function (api) {
      try {
        (function () {
          document.getElementById("search-term").placeholder = "here..";
        });
      } catch (err) {
        var rescue = require("discourse/lib/utilities").rescueThemeError;

        rescue(__theme_name__, err, api);
      }
    });
  }
})();