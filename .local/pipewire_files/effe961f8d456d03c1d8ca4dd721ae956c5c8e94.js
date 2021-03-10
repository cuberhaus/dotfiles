(function() {
  if ('Discourse' in window && Discourse.__container__) {
    Discourse.__container__
      .lookup("service:theme-settings")
      .registerSettings(4, {"Heading":"Hello Community","Blurb":"Have fun tinkering, share knowledge and respect other members.","Link_sections":"Lists, All related mailing lists!|Support, Learn new things!|Distro, Info related with the distribution itself.|Support Us","Links":"Distro, Mirrors, https://repo.manjaro.org/, blank, Repositories status. |Distro, Branch Compare, https://manjaro.org/branch-compare/, blank, Compare software versions in different branches.|Distro, Discover, https://discover.manjaro.org/, blank, Explore and install available software.|Support, First Steps, https://manjaro.org/support/firststeps/, blank, Small guide that covers the basics.|Support, Common Issues, https://manjaro.org/support/commonproblems/, blank, Some issues you may encounter.|Support, User Guide, https://manjaro.org/support/userguide/, blank, User guide in English/French/Turkish.|Support, Wiki, https://wiki.manjaro.org/index.php, blank, Extensive guide.|Lists, Security, https://lists.manjaro.org/mailman/listinfo/manjaro-security, blank, mailing list|Lists, General, https://lists.manjaro.org/mailman/listinfo/manjaro-general, blank, mailing list|Lists, Testing, https://lists.manjaro.org/mailman/listinfo/manjaro-testing, blank, mailing list|Lists, Development, https://lists.manjaro.org/mailman/listinfo/manjaro-dev, blank, mailing list|Lists, Mirrors, https://lists.manjaro.org/mailman/listinfo/manjaro-mirrors, blank, mailing list|Support Us, Donate, https://manjaro.org/donate/, blank|Support Us, Merch \u0026 Accessories, https://shop.spreadshirt.de/manjaro/, blank|Support Us, Hardware, https://manjaro.org/hardware, blank|Support, Archived Forum, http://archived.forum.manjaro.org, blank, Old forum content.|Support Us, Donate ARM, https://www.patreon.com/bePatron?u=12307494, blank|Support Us, Stickers, https://linux-aarhus.dk/en/product-category/manjaro-en/stickers-en/, blank|Distro, Downloads, https://manjaro.org/download/, blank","Small_links":"Privacy Policy, https://manjaro.org/privacy-policy/, blank|Terms of use, https://manjaro.org/terms-of-use/, blank|Imprint, https://manjaro.org/imprint/, blank|Team, https://manjaro.org/team/, blank","Social_links":"Twitter, show some love on Twitter, https://twitter.com/ManjaroLinux, blank,fab-twitter| Youtube, Check out our latest videos on Youtube, https://www.youtube.com/channel/UCdGFLV7h9RGeTUX7wa5rqGw, blank,fab-youtube|Gitlab, Code repository, https://gitlab.manjaro.org/, blank, fab-gitlab","svg_icons":"fab-facebook|fab-twitter|fab-youtube|fab-gitlab"});
  }
})();
(function() {
  if ('Ember' in window) {
    Ember.TEMPLATES["discourse/connectors/below-footer/custom-footer"] = Ember.HTMLBars.template({"id":null,"block":"{\"symbols\":[\"link\",\"link\",\"section\",\"link\"],\"statements\":[[4,\"if\",[[24,[\"showFooter\"]]],null,{\"statements\":[[0,\"  \"],[7,\"div\",true],[10,\"class\",\"wrap\"],[8],[0,\"\\n    \"],[7,\"div\",true],[10,\"class\",\"flexbox\"],[8],[0,\"\\n      \"],[7,\"div\",true],[10,\"class\",\"first-box\"],[8],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"heading\"],[8],[0,\"\\n          \"],[1,[22,\"mainHeading\"],false],[0,\"\\n        \"],[9],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"blurb\"],[8],[0,\"\\n          \"],[1,[22,\"blurb\"],false],[0,\"\\n        \"],[9],[0,\"\\n      \"],[9],[0,\"\\n      \"],[7,\"div\",true],[10,\"class\",\"second-box\"],[8],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"links\"],[8],[0,\"\\n\"],[4,\"each\",[[24,[\"linkSections\"]]],null,{\"statements\":[[0,\"            \"],[7,\"div\",true],[10,\"class\",\"list\"],[8],[0,\"\\n              \"],[7,\"span\",true],[11,\"class\",[29,[[23,3,[\"className\"]]]]],[11,\"title\",[29,[[23,3,[\"title\"]]]]],[8],[0,\"\\n                \"],[1,[23,3,[\"text\"]],false],[0,\"\\n              \"],[9],[0,\"\\n              \"],[7,\"ul\",true],[8],[0,\"\\n\"],[4,\"each\",[[23,3,[\"childLinks\"]]],null,{\"statements\":[[0,\"                  \"],[7,\"li\",true],[11,\"class\",[29,[\"footer-section-link-wrapper \",[23,4,[\"className\"]]]]],[8],[0,\"\\n                    \"],[7,\"a\",true],[10,\"class\",\"footer-section-link\"],[11,\"title\",[29,[[23,4,[\"title\"]]]]],[11,\"href\",[29,[[23,4,[\"href\"]]]]],[11,\"target\",[29,[[23,4,[\"target\"]]]]],[8],[0,\"\\n                      \"],[1,[23,4,[\"text\"]],false],[0,\"\\n                    \"],[9],[0,\"\\n                  \"],[9],[0,\"\\n\"]],\"parameters\":[4]},null],[0,\"              \"],[9],[0,\"\\n            \"],[9],[0,\"\\n\"]],\"parameters\":[3]},null],[0,\"        \"],[9],[0,\"\\n      \"],[9],[0,\"\\n      \"],[7,\"div\",true],[10,\"class\",\"third-box\"],[8],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"footer-links\"],[8],[0,\"\\n\"],[4,\"each\",[[24,[\"smallLinks\"]]],null,{\"statements\":[[0,\"            \"],[7,\"a\",true],[11,\"class\",[29,[\"small-link \",[23,2,[\"className\"]]]]],[11,\"title\",[29,[[23,2,[\"title\"]]]]],[11,\"target\",[29,[[23,2,[\"target\"]]]]],[11,\"href\",[29,[[23,2,[\"href\"]]]]],[8],[1,[23,2,[\"text\"]],false],[9],[0,\"\\n\"]],\"parameters\":[2]},null],[0,\"        \"],[9],[0,\"\\n        \"],[7,\"div\",true],[10,\"class\",\"social\"],[8],[0,\"\\n\"],[4,\"each\",[[24,[\"socialLinks\"]]],null,{\"statements\":[[0,\"            \"],[7,\"a\",true],[11,\"class\",[29,[\"social-link \",[23,1,[\"className\"]]]]],[11,\"title\",[29,[[23,1,[\"title\"]]]]],[11,\"target\",[29,[[23,1,[\"target\"]]]]],[11,\"href\",[29,[[23,1,[\"href\"]]]]],[8],[1,[28,\"d-icon\",[[23,1,[\"icon\"]]],null],false],[9],[0,\"\\n\"]],\"parameters\":[1]},null],[0,\"        \"],[9],[0,\"\\n      \"],[9],[0,\"\\n    \"],[9],[0,\"\\n  \"],[9],[0,\"\\n\"]],\"parameters\":[]},null]],\"hasEval\":false}","meta":{}});
  }
})();

define("discourse/connectors/below-footer/custom-footer", ["exports"], function (_exports) {
  "use strict";

  Object.defineProperty(_exports, "__esModule", {
    value: true
  });
  _exports.default = void 0;
  var __theme_name__ = "Easy Footer";

  var settings = Discourse.__container__.lookup("service:theme-settings").getObjectForTheme(4);

  var themePrefix = function themePrefix(key) {
    return "theme_translations.4.".concat(key);
  }; // Used instead of dasherize for backwards compatibility with stable


  var getClassName = function getClassName(text) {
    return text.toLowerCase().replace(/\s/g, "-");
  };

  var _default = {
    setupComponent: function setupComponent(args, component) {
      try {
        var splitLinkSections = settings.Link_sections.split("|").filter(Boolean);
        var splitLinks = settings.Links.split("|").filter(Boolean);
        var splitSmallLinks = settings.Small_links.split("|").filter(Boolean);
        var splitSocialLinks = settings.Social_links.split("|").filter(Boolean);
        var linkArray = [];
        var sectionsArray = [];
        var smallLinksArray = [];
        var socialLinksArray = [];
        splitLinks.forEach(function (link) {
          var fragments = link.split(",").map(function (fragment) {
            return fragment.trim();
          });
          var parent = fragments[0].toLowerCase();
          var text = fragments[1];
          var className = getClassName(text);
          var href = fragments[2];
          var target = fragments[3] === "blank" ? "_blank" : "";
          var title = fragments[4];
          var linkItem = {
            parent: parent,
            text: text,
            className: className,
            href: href,
            target: target,
            title: title
          };
          linkArray.push(linkItem);
        });
        splitLinkSections.forEach(function (section) {
          var fragments = section.split(",").map(function (fragment) {
            return fragment.trim();
          });
          var parentFor = fragments[0].toLowerCase();
          var text = fragments[0];
          var className = getClassName(text);
          var title = fragments[1];
          var childLinks = linkArray.filter(function (link) {
            return link.parent === parentFor;
          });
          var listItem = {
            text: text,
            className: className,
            childLinks: childLinks
          };
          sectionsArray.push(listItem);
        });
        splitSocialLinks.forEach(function (link) {
          var fragments = link.split(",").map(function (fragment) {
            return fragment.trim();
          });
          var text = fragments[0];
          var className = getClassName(text);
          var title = fragments[1];
          var href = fragments[2];
          var target = fragments[3] === "blank" ? "_blank" : "";
          var icon = fragments[4].toLowerCase();
          var socialLinkItem = {
            text: text,
            className: className,
            title: title,
            href: href,
            target: target,
            icon: icon
          };
          socialLinksArray.push(socialLinkItem);
        });
        splitSmallLinks.forEach(function (link) {
          var fragments = link.split(",").map(function (fragment) {
            return fragment.trim();
          });
          var text = fragments[0];
          var className = getClassName(text);
          var href = fragments[1];
          var target = fragments[2] === "blank" ? "_blank" : "";
          var smallLinkItem = {
            text: text,
            className: className,
            href: href,
            target: target
          };
          smallLinksArray.push(smallLinkItem);
        });
        this.setProperties({
          mainHeading: settings.Heading,
          blurb: settings.Blurb,
          linkSections: sectionsArray,
          smallLinks: smallLinksArray,
          socialLinks: socialLinksArray
        });
      } catch (error) {
        console.error(error);
        console.error("There's an issue in the Easy Footer Component. Check if your settings are entered correctly");
      }
    }
  };
  _exports.default = _default;
});
