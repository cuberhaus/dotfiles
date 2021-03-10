// Instantiate the object
var I18n = I18n || {};

// Set default locale to english
I18n.defaultLocale = "en";

// Set default pluralization rule
I18n.pluralizationRules = {
  en: function(n) {
    return n === 0 ? ["zero", "none", "other"] : n === 1 ? "one" : "other";
  }
};

// Set current locale to null
I18n.locale = null;
I18n.fallbackLocale = null;

// Set the placeholder format. Accepts `{{placeholder}}` and `%{placeholder}`.
I18n.PLACEHOLDER = /(?:\{\{|%\{)(.*?)(?:\}\}?)/gm;

I18n.SEPARATOR = ".";

I18n.noFallbacks = false;

I18n.isValidNode = function(obj, node, undefined) {
  return obj[node] !== null && obj[node] !== undefined;
};

I18n.lookup = function(scope, options) {
  options = options || {};

  var translations = this.prepareOptions(I18n.translations),
    locale = options.locale || I18n.currentLocale(),
    messages = translations[locale] || {},
    currentScope;

  options = this.prepareOptions(options);

  if (typeof scope === "object") {
    scope = scope.join(this.SEPARATOR);
  }

  if (options.scope) {
    scope = options.scope.toString() + this.SEPARATOR + scope;
  }

  var originalScope = scope;
  scope = scope.split(this.SEPARATOR);

  if (scope.length > 0 && scope[0] !== "js") {
    scope.unshift("js");
  }

  while (messages && scope.length > 0) {
    currentScope = scope.shift();
    messages = messages[currentScope];
  }

  if (messages === undefined && this.extras && this.extras[locale]) {
    messages = this.extras[locale];
    scope = originalScope.split(this.SEPARATOR);

    while (messages && scope.length > 0) {
      currentScope = scope.shift();
      messages = messages[currentScope];
    }
  }

  if (messages === undefined) {
    messages = options.defaultValue;
  }

  return messages;
};

// Merge serveral hash options, checking if value is set before
// overwriting any value. The precedence is from left to right.
//
//   I18n.prepareOptions({name: "John Doe"}, {name: "Mary Doe", role: "user"});
//   #=> {name: "John Doe", role: "user"}
//
I18n.prepareOptions = function() {
  var options = {},
    opts,
    count = arguments.length;

  for (var i = 0; i < count; i++) {
    opts = arguments[i];

    if (!opts) {
      continue;
    }

    for (var key in opts) {
      if (!this.isValidNode(options, key)) {
        options[key] = opts[key];
      }
    }
  }

  return options;
};

I18n.interpolate = function(message, options) {
  options = this.prepareOptions(options);

  var matches = message.match(this.PLACEHOLDER),
    placeholder,
    value,
    name;

  if (!matches) {
    return message;
  }

  for (var i = 0; (placeholder = matches[i]); i++) {
    name = placeholder.replace(this.PLACEHOLDER, "$1");

    if (typeof options[name] === "string") {
      // The dollar sign (`$`) is a special replace pattern, and `$&` inserts
      // the matched string. Thus dollars signs need to be escaped with the
      // special pattern `$$`, which inserts a single `$`.
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace#Specifying_a_string_as_a_parameter
      value = options[name].replace(/\$/g, "$$$$");
    } else {
      value = options[name];
    }

    if (!this.isValidNode(options, name)) {
      value = "[missing " + placeholder + " value]";
    }

    var regex = new RegExp(
      placeholder.replace(/\{/gm, "\\{").replace(/\}/gm, "\\}")
    );
    message = message.replace(regex, value);
  }

  return message;
};

I18n.translate = function(scope, options) {
  options = this.prepareOptions(options);
  options.needsPluralization = typeof options.count === "number";
  options.ignoreMissing = !this.noFallbacks;

  var translation = this.findTranslation(scope, options);

  if (!this.noFallbacks) {
    if (!translation && this.fallbackLocale) {
      options.locale = this.fallbackLocale;
      translation = this.findTranslation(scope, options);
    }

    options.ignoreMissing = false;

    if (!translation && this.currentLocale() !== this.defaultLocale) {
      options.locale = this.defaultLocale;
      translation = this.findTranslation(scope, options);
    }

    if (!translation && this.currentLocale() !== "en") {
      options.locale = "en";
      translation = this.findTranslation(scope, options);
    }
  }

  try {
    return this.interpolate(translation, options);
  } catch (error) {
    return this.missingTranslation(scope);
  }
};

I18n.findTranslation = function(scope, options) {
  var translation = this.lookup(scope, options);

  if (translation && options.needsPluralization) {
    translation = this.pluralize(translation, scope, options);
  }

  return translation;
};

I18n.toNumber = function(number, options) {
  options = this.prepareOptions(options, this.lookup("number.format"), {
    precision: 3,
    separator: this.SEPARATOR,
    delimiter: ",",
    strip_insignificant_zeros: false
  });

  var negative = number < 0,
    string = Math.abs(number)
      .toFixed(options.precision)
      .toString(),
    parts = string.split(this.SEPARATOR),
    buffer = [],
    formattedNumber;

  number = parts[0];

  while (number.length > 0) {
    buffer.unshift(number.substr(Math.max(0, number.length - 3), 3));
    number = number.substr(0, number.length - 3);
  }

  formattedNumber = buffer.join(options.delimiter);

  if (options.precision > 0) {
    formattedNumber += options.separator + parts[1];
  }

  if (negative) {
    formattedNumber = "-" + formattedNumber;
  }

  if (options.strip_insignificant_zeros) {
    var regex = {
      separator: new RegExp(options.separator.replace(/\./, "\\.") + "$"),
      zeros: /0+$/
    };

    formattedNumber = formattedNumber
      .replace(regex.zeros, "")
      .replace(regex.separator, "");
  }

  return formattedNumber;
};

I18n.toHumanSize = function(number, options) {
  var kb = 1024,
    size = number,
    iterations = 0,
    unit,
    precision;

  while (size >= kb && iterations < 4) {
    size = size / kb;
    iterations += 1;
  }

  if (iterations === 0) {
    unit = this.t("number.human.storage_units.units.byte", { count: size });
    precision = 0;
  } else {
    unit = this.t(
      "number.human.storage_units.units." +
        [null, "kb", "mb", "gb", "tb"][iterations]
    );
    precision = size - Math.floor(size) === 0 ? 0 : 1;
  }

  options = this.prepareOptions(options, {
    precision: precision,
    format: this.t("number.human.storage_units.format"),
    delimiter: ""
  });

  number = this.toNumber(size, options);
  number = options.format.replace("%u", unit).replace("%n", number);

  return number;
};

I18n.pluralizer = function(locale) {
  var pluralizer = this.pluralizationRules[locale];
  if (pluralizer !== undefined) return pluralizer;
  return this.pluralizationRules["en"];
};

I18n.findAndTranslateValidNode = function(keys, translation) {
  for (var i = 0; i < keys.length; i++) {
    var key = keys[i];
    if (this.isValidNode(translation, key)) return translation[key];
  }
  return null;
};

I18n.pluralize = function(translation, scope, options) {
  if (typeof translation !== "object") return translation;

  options = this.prepareOptions(options);
  var count = options.count.toString();

  var pluralizer = this.pluralizer(options.locale || this.currentLocale());
  var key = pluralizer(Math.abs(count));
  var keys = typeof key === "object" && key instanceof Array ? key : [key];

  var message = this.findAndTranslateValidNode(keys, translation);

  if (message !== null || options.ignoreMissing) {
    return message;
  }

  return this.missingTranslation(scope, keys[0]);
};

I18n.missingTranslation = function(scope, key) {
  var message = "[" + this.currentLocale() + this.SEPARATOR + scope;
  if (key) {
    message += this.SEPARATOR + key;
  }
  return message + "]";
};

I18n.currentLocale = function() {
  return I18n.locale || I18n.defaultLocale;
};

I18n.enableVerboseLocalization = function() {
  var counter = 0;
  var keys = {};
  var t = I18n.t;

  I18n.noFallbacks = true;

  I18n.t = I18n.translate = function(scope, value) {
    var current = keys[scope];
    if (!current) {
      current = keys[scope] = ++counter;
      var message = "Translation #" + current + ": " + scope;
      if (value && Object.keys(value).length > 0) {
        message += ", parameters: " + JSON.stringify(value);
      }
      // eslint-disable-next-line no-console
      console.info(message);
    }
    return t.apply(I18n, [scope, value]) + " (#" + current + ")";
  };
};

I18n.enableVerboseLocalizationSession = function() {
  sessionStorage.setItem("verbose_localization", "true");
  I18n.enableVerboseLocalization();

  return "Verbose localization is enabled. Close the browser tab to turn it off. Reload the page to see the translation keys.";
};

// shortcuts
I18n.t = I18n.translate;


MessageFormat = {locale: {}};
I18n._compiledMFs = {"too_few_topics_and_posts_notice_MF" : function(d){
var r = "";
r += "<a href=\"https://blog.discourse.org/2014/08/building-a-discourse-community/\">¡Comencemos la discusión!</a> Hay ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "currentTopics";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> tema";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " y ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "currentPosts";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicación";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Los visitantes necesitan más contenido para leer y responder – nosotros recomendamos al menos ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "requiredTopics";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> tema";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " y ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "requiredPosts";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicación";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Solo los miembros del staff pueden ver este mensaje.";
return r;
}, "too_few_topics_notice_MF" : function(d){
var r = "";
r += "<a href=\"https://blog.discourse.org/2014/08/building-a-discourse-community/\">¡Comencemos la discusión!</a> Hay ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "currentTopics";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> tema";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Los visitantes necesitan más contenido para leer y responder – nosotros recomendamos al menos ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "requiredTopics";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> tema";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Solo los miembros del staff pueden ver este mensaje.";
return r;
}, "too_few_posts_notice_MF" : function(d){
var r = "";
r += "<a href=\"https://blog.discourse.org/2014/08/building-a-discourse-community/\">¡Comencemos la discusión!</a> Hay ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "currentPosts";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicación";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Los visitantes necesitan más contenido para leer y responder – nosotros recomendamos al menos ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "requiredPosts";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicación";
return r;
},
"other" : function(d){
var r = "";
r += "<strong>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</strong> publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". Solo los miembros del staff pueden ver este mensaje.";
return r;
}, "logs_error_rate_notice.reached_hour_MF" : function(d){
var r = "";
r += "<b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["relativeAge"];
r += "</b> – <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["url"];
r += "' target='_blank'>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "rate";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/hour";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/hour";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += "</a> alcanzó el límite de la configuración del sitio de ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "limit";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/hour";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/hour";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ".";
return r;
}, "logs_error_rate_notice.reached_minute_MF" : function(d){
var r = "";
r += "<b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["relativeAge"];
r += "</b> – <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["url"];
r += "' target='_blank'>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "rate";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/minute";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/minute";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += "</a> alcanzó el límite de la configuración del sitio de ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "limit";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/minute";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/minute";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ".";
return r;
}, "logs_error_rate_notice.exceeded_hour_MF" : function(d){
var r = "";
r += "<b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["relativeAge"];
r += "</b> – <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["url"];
r += "' target='_blank'>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "rate";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/hour";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/hour";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += "</a> excedió el límite de la configuración del sitio del ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "limit";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/hour";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/hour";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ".";
return r;
}, "logs_error_rate_notice.exceeded_minute_MF" : function(d){
var r = "";
r += "<b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["relativeAge"];
r += "</b> – <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["url"];
r += "' target='_blank'>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "rate";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/minute";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/minute";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += "</a> excedió el límite de la configuración del sitio de ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "limit";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " error/minute";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " errors/minute";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ".";
return r;
}, "summary.description_time_MF" : function(d){
var r = "";
r += "Hay ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "replyCount";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "Hay <b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> respuesta";
return r;
},
"other" : function(d){
var r = "";
r += "Hay <b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> respuestas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " con un tiempo estimado de lectura de <b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "readingTime";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " minuto";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " minutos";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += "</b>.";
return r;
}, "topic.bumped_at_title_MF" : function(d){
var r = "";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["FIRST_POST"];
r += ": ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["CREATED_AT"];
r += "\n";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["LAST_POST"];
r += ": ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["BUMPED_AT"];
return r;
}, "flagging.delete_confirm_MF" : function(d){
var r = "";
r += "Estás a punto de eliminar ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "POSTS";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> publicación";
return r;
},
"other" : function(d){
var r = "";
r += "<b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " y ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "TOPICS";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "<b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> tema";
return r;
},
"other" : function(d){
var r = "";
r += "<b>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + "</b> temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " de este usuario, también eliminarás su cuenta, bloquearás registros desde su dirección IP <b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["ip_address"];
r += "</b>, y añadirás su correo electrónico <b>";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["email"];
r += "</b> a una lista de bloqueos permanentes. ¿Seguro que el usuario es de verdad un spammer?";
return r;
}, "posts_likes_MF" : function(d){
var r = "";
r += "Este tema tiene ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "count";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " respuesta";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " respuestas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "ratio";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"low" : function(d){
var r = "";
r += "con una proporción de me gusta por publicación alta";
return r;
},
"med" : function(d){
var r = "";
r += "con una proporción de me gusta por publicación muy alta";
return r;
},
"high" : function(d){
var r = "";
r += "con una proporción de me gusta por publicación extremadamente alta";
return r;
},
"other" : function(d){
var r = "";
return r;
}
};
r += (pf_0[ k_1 ] || pf_0[ "other" ])( d );
r += "\n";
return r;
}, "admin.user.delete_all_posts_confirm_MF" : function(d){
var r = "";
r += "Estás a punto de eliminar ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "POSTS";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " publicación";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " publicaciones";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " y ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "TOPICS";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"one" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " tema";
return r;
},
"other" : function(d){
var r = "";
r += "" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " temas";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += ". ¿Estás seguro?";
return r;
}, "topic.read_more_MF" : function(d){
var r = "";
r += "There ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "UNREAD";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"0" : function(d){
var r = "";
return r;
},
"one" : function(d){
var r = "";
r += "is <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["basePath"];
r += "/unread'>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " unread</a> ";
return r;
},
"other" : function(d){
var r = "";
r += "are <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["basePath"];
r += "/unread'>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " unread</a> ";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "NEW";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"0" : function(d){
var r = "";
return r;
},
"one" : function(d){
var r = "";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_2 = "BOTH";
var k_2=d[lastkey_2];
var off_1 = 0;
var pf_1 = { 
"true" : function(d){
var r = "";
r += "and ";
return r;
},
"false" : function(d){
var r = "";
r += "is ";
return r;
},
"other" : function(d){
var r = "";
return r;
}
};
r += (pf_1[ k_2 ] || pf_1[ "other" ])( d );
r += " <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["basePath"];
r += "/new'>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " new</a> topic";
return r;
},
"other" : function(d){
var r = "";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_2 = "BOTH";
var k_2=d[lastkey_2];
var off_1 = 0;
var pf_1 = { 
"true" : function(d){
var r = "";
r += "and ";
return r;
},
"false" : function(d){
var r = "";
r += "are ";
return r;
},
"other" : function(d){
var r = "";
return r;
}
};
r += (pf_1[ k_2 ] || pf_1[ "other" ])( d );
r += " <a href='";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["basePath"];
r += "/new'>" + (function(){ var x = k_1 - off_0;
if( isNaN(x) ){
throw new Error("MessageFormat: `"+lastkey_1+"` isnt a number.");
}
return x;
})() + " new</a> topics";
return r;
}
};
if ( pf_0[ k_1 + "" ] ) {
r += pf_0[ k_1 + "" ]( d ); 
}
else {
r += (pf_0[ MessageFormat.locale["es"]( k_1 - off_0 ) ] || pf_0[ "other" ] )( d );
}
r += " remaining, or ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
var lastkey_1 = "CATEGORY";
var k_1=d[lastkey_1];
var off_0 = 0;
var pf_0 = { 
"true" : function(d){
var r = "";
r += "browse other topics in ";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["catLink"];
return r;
},
"false" : function(d){
var r = "";
if(!d){
throw new Error("MessageFormat: No data passed to function.");
}
r += d["latestLink"];
return r;
},
"other" : function(d){
var r = "";
return r;
}
};
r += (pf_0[ k_1 ] || pf_0[ "other" ])( d );
return r;
}};
MessageFormat.locale.es = function ( n ) {
  if ( n === 1 ) {
    return "one";
  }
  return "other";
};

(function() {
  I18n.messageFormat = function(key, options) {
    var fn = I18n._compiledMFs[key];
    if (fn) {
      try {
        return fn(options);
      } catch (err) {
        return err.message;
      }
    } else {
      return "Missing Key: " + key;
    }
  };
})();

I18n.translations = {"es":{"js":{"number":{"format":{"separator":",","delimiter":"."},"human":{"storage_units":{"format":"%n %u","units":{"byte":{"one":"Byte","other":"Bytes"},"gb":"GB","kb":"KB","mb":"MB","tb":"TB"}}},"short":{"thousands":"%{number}k","millions":"%{number}M"}},"dates":{"time":"HH:mm","time_with_zone":"HH:mm (z)","time_short_day":"ddd, HH:mm","timeline_date":"MMM YYYY","long_no_year":"D MMM, HH:mm","long_no_year_no_time":"D MMM","full_no_year_no_time":"Do MMMM","long_with_year":"D MMM YYYY HH:mm","long_with_year_no_time":"D MMM YYYY","full_with_year_no_time":"D MMMM YYYY","long_date_with_year":"D MMM 'YY LT","long_date_without_year":"D MMM LT","long_date_with_year_without_time":"D MMM 'YY","long_date_without_year_with_linebreak":"D MMM \u003cbr/\u003eLT","long_date_with_year_with_linebreak":"D MMM 'YY \u003cbr/\u003eLT","wrap_ago":"hace %{date}","tiny":{"half_a_minute":"\u003c 1 m","less_than_x_seconds":{"one":"\u003c %{count}s","other":"\u003c %{count} s"},"x_seconds":{"one":"%{count}s","other":"%{count}s"},"less_than_x_minutes":{"one":"\u003c %{count} min","other":"\u003c %{count} min"},"x_minutes":{"one":"%{count} min","other":"%{count} min"},"about_x_hours":{"one":"%{count} h","other":"%{count} h"},"x_days":{"one":"%{count} d","other":"%{count} d"},"x_months":{"one":"%{count} mes","other":"%{count} meses"},"about_x_years":{"one":"%{count}a","other":"%{count} a"},"over_x_years":{"one":"\u003e %{count}a","other":"\u003e %{count} a"},"almost_x_years":{"one":"%{count}a","other":"%{count} a"},"date_month":"D MMM","date_year":"MMM 'YY"},"medium":{"x_minutes":{"one":"%{count} minuto","other":"%{count} min"},"x_hours":{"one":"%{count} hora","other":"%{count} horas"},"x_days":{"one":"%{count} día","other":"%{count} días"},"date_year":"D MMM 'YY"},"medium_with_ago":{"x_minutes":{"one":"hace %{count} minuto","other":"hace %{count} minutos"},"x_hours":{"one":"hace %{count} hora","other":"hace %{count} horas"},"x_days":{"one":"hace %{count} día","other":"hace %{count} días"},"x_months":{"one":"hace %{count} mes","other":"hace %{count} meses"},"x_years":{"one":"hace %{count} año","other":"hace %{count} años"}},"later":{"x_days":{"one":"%{count} día después","other":"%{count} días después"},"x_months":{"one":"%{count} mes después","other":"%{count} meses después"},"x_years":{"one":"%{count} año después","other":"%{count} años después"}},"previous_month":"Mes anterior","next_month":"Mes siguiente","placeholder":"fecha"},"share":{"topic_html":"Tema: \u003cspan class=\"topic-title\"\u003e%{topicTitle}\u003c/span\u003e","post":"publicación núm. %{postNumber}","close":"cerrar","twitter":"Compartir en Twitter","facebook":"Compartir en Facebook","email":"Enviar por correo electrónico","url":"Copiar y compartir la URL"},"action_codes":{"public_topic":"hizo este tema público %{when}","private_topic":"hizo este tema un mensaje personal %{when}","split_topic":"separó este tema %{when}","invited_user":"invitó a %{who} %{when}","invited_group":"invitó a %{who} %{when}","user_left":"%{who} se eliminó a sí mismo de este mensaje %{when}","removed_user":"eliminó a %{who} %{when}","removed_group":"eliminó a %{who} %{when}","autobumped":"bumped automáticamente %{when}","autoclosed":{"enabled":"cerrado %{when}","disabled":"abierto %{when}"},"closed":{"enabled":"cerrado %{when}","disabled":"abierto %{when}"},"archived":{"enabled":"archivado %{when}","disabled":"desarchivado %{when}"},"pinned":{"enabled":"fijado %{when}","disabled":"desfijado %{when}"},"pinned_globally":{"enabled":"fijado globalmente %{when}","disabled":"sin destacar %{when}"},"visible":{"enabled":"listado %{when}","disabled":"quitado de la lista %{when}"},"banner":{"enabled":"hizo esto un encabezado %{when}. Aparecerá en la parte superior de cada página hasta que el usuario lo descarte.","disabled":"eliminó este encabezado %{when}. Ya no aparecerá en la parte superior de cada página."},"forwarded":"reenvió el correo electrónico de arriba"},"topic_admin_menu":"acciones del tema","wizard_required":"¡Bienvenido a tu nuevo Discourse! Empezaremos con \u003ca href='%{url}' data-auto-route='true'\u003eel asistente de configuración\u003c/a\u003e ✨","emails_are_disabled":"Todos los correos electrónicos salientes han sido deshabilitados globalmente por un administrador. No se enviarán notificaciones por correo electrónico de ningún tipo.","bootstrap_mode_enabled":{"one":"Para hacer que la inauguración de tu sitio sea más fácil, el modo lanzamiento está activado. Todas las cuentas tendrán nivel de confianza 1 y los mensajes de resumen diarios están activados. Esto se desactivará automáticamente cuando se hayan registrado %{count} usuario.","other":"Para hacer que la inauguración de tu sitio sea más fácil, el modo lanzamiento está activado. Todas las cuentas tendrán nivel de confianza 1 y los mensajes de resumen diarios están activados. Esto se desactivará automáticamente cuando se hayan registrado %{count} usuarios."},"bootstrap_mode_disabled":"El modo de arranque se desactivará dentro de las próximas 24 horas.","themes":{"default_description":"Por defecto","broken_theme_alert":"Tu sitio puede no funcionar porque el tema / componente %{theme} tiene errores. Desactívalo en %{path}."},"s3":{"regions":{"ap_northeast_1":"Asia-Pacífico (Tokio)","ap_northeast_2":"Asia-Pacífico (Seúl)","ap_south_1":"Asia Pacific (Mumbai)","ap_southeast_1":"Asia-Pacífico (Singapur)","ap_southeast_2":"Asia-Pacífico (Sydney)","ca_central_1":"Canadá (Central)","cn_north_1":"China (Beijing)","cn_northwest_1":"China (Ningxia)","eu_central_1":"EU (Frankfurt)","eu_north_1":"EU (Estocolmo)","eu_west_1":"EU (Irlanda)","eu_west_2":"EU (Londres)","eu_west_3":"EU (París)","sa_east_1":"Sudamérica (São Paulo)","us_east_1":"EEUU este (norte de Virginia)","us_east_2":"EEUU este (Ohio)","us_gov_east_1":"AWS GovCloud (EEUU-este)","us_gov_west_1":"AWS GovCloud (EEUU-oeste)","us_west_1":"EEUU oeste (norte de California)","us_west_2":"EEUU oeste (Oregon)"}},"clear_input":"Borrar","edit":"editar el título y la categoría de este tema","expand":"Expandir","not_implemented":"¡Lo sentimos! Esa característica no se ha implementado todavía.","no_value":"No","yes_value":"Sí","submit":"Enviar","generic_error":"Lo sentimos, ha ocurrido un error.","generic_error_with_reason":"Ha ocurrido un error: %{error}","go_ahead":"Continuar","sign_up":"Registrarse","log_in":"Iniciar sesión","age":"Edad","joined":"Registrado","admin_title":"Admin","show_more":"mostrar más","show_help":"opciones","links":"Enlaces","links_lowercase":{"one":"enlace","other":"enlaces"},"faq":"Preguntas frecuentes","guidelines":"Guía","privacy_policy":"Política de privacidad","privacy":"Privacidad","tos":"Términos del servicio","rules":"Reglas","conduct":"Código de conducta","mobile_view":"Versión móvil","desktop_view":"Versión de escritorio","you":"Tú","or":"o","now":"justo ahora","read_more":"leer más","more":"Más","less":"Menos","never":"nunca","every_30_minutes":"cada 30 minutos","every_hour":"cada hora","daily":"diario","weekly":"semanalmente","every_month":"cada mes","every_six_months":"cada seis meses","max_of_count":"máximo de %{count}","alternation":"o","character_count":{"one":"%{count} carácter","other":"%{count} caracteres"},"related_messages":{"title":"Mensajes relacionados","see_all":"Ver \u003ca href=\"%{path}\"\u003etodos los mensajes\u003c/a\u003e de @%{username}..."},"suggested_topics":{"title":"Temas sugeridos","pm_title":"Mensajes sugeridos"},"about":{"simple_title":"Acerca de","title":"Sobre %{title}","stats":"Estadísticas del sitio","our_admins":"Nuestros administradores","our_moderators":"Nuestros moderadores","moderators":"Moderadores","stat":{"all_time":"Todo el tiempo","last_7_days":"Últimos 7","last_30_days":"Últimos 30"},"like_count":"Me gusta","topic_count":"Temas","post_count":"Publicaciones","user_count":"Usuarios","active_user_count":"Usuarios activos","contact":"Contáctanos","contact_info":"En caso de un problema crítico o urgente que esté afectando este sitio, contáctanos a través de %{contact_info}."},"bookmarked":{"title":"Marcador","clear_bookmarks":"Quitar marcadores","help":{"bookmark":"Haz clic para añadir a marcadores la primera publicación de este tema","unbookmark":"Haz clic para eliminar todos los marcadores de este tema","unbookmark_with_reminder":"Haz clic para quitar todos los marcadores y recordatorios en este tema. Tienes un recordatorio puesto %{reminder_at} para este tema."}},"bookmarks":{"created":"Has guardado esta publicación en marcadores. %{name}","not_bookmarked":"añadir esta publicación a marcadores","created_with_reminder":"Has marcado esta publicación con un recordatorio %{date}. %{name}","remove":"Eliminar marcador","delete":"Borrar marcador","confirm_delete":"¿Seguro que quieres borrar este marcados? El recordatorio también se borrará.","confirm_clear":"¿Estás seguro de que deseas eliminar todos tus marcadores en este tema?","save":"Guardar","no_timezone":"No has establecido una zona horaria todavía. No podrás establecer recordatorios. Puedes elegir una \u003ca href=\"%{basePath}/my/preferences/profile\"\u003een tu perfil\u003c/a\u003e.","invalid_custom_datetime":"La fecha y hora que suministraste es inválida. Por favor, intenta de nuevo.","list_permission_denied":"No tienes permiso para ver los marcadores de este usuario.","no_user_bookmarks":"No tiene publicaciones marcadas; los marcadores le permiten referirse rápidamente a publicaciones específicas.","auto_delete_preference":{"label":"Borrar automáticamente","never":"Nunca","when_reminder_sent":"Cuando se envíe el recordatorio","on_owner_reply":"Cuando responda a este tema"},"search_placeholder":"Buscar marcadores por nombre, título del tema o contenido de la publicación","search":"Buscar","reminders":{"later_today":"Más tarde durante el día de hoy","next_business_day":"Siguiente día hábil","tomorrow":"Mañana","next_week":"Próxima semana","post_local_date":"Fecha de la publicación","later_this_week":"Esta misma semana","start_of_next_business_week":"Lunes","start_of_next_business_week_alt":"Lunes que viene","next_month":"Próximo mes","custom":"Fecha y hora personalizadas","last_custom":"Último","none":"No necesita recordatorio","today_with_time":"hoy a las %{time}","tomorrow_with_time":"mañana a las %{time}","at_time":"a las %{date_time}","existing_reminder":"Tienes un recordatorio para este marcador que se enviará %{at_date_time}"}},"copy_codeblock":{"copied":"¡copiado!"},"drafts":{"resume":"Reanudar","remove":"Eliminar","remove_confirmation":"¿Seguro que quieres eliminar este borrador?","new_topic":"Nuevo borrador de tema","new_private_message":"Nuevo borrador de mensaje privado","topic_reply":"Borrador de respuesta","abandon":{"confirm":"Ya has abierto otro borrador en este tema. ¿Estás seguro de que quieres abandonarlo?","yes_value":"Sí, abandonar","no_value":"No, mantener"}},"topic_count_latest":{"one":"Ver %{count} tema nuevo o actualizado","other":"Ver %{count} temas nuevos o actualizados"},"topic_count_unread":{"one":"Ver %{count} tema sin leer","other":"Ver %{count} temas no leídos"},"topic_count_new":{"one":"Ver %{count} tema nuevo","other":"Ver %{count} temas nuevos"},"preview":"vista previa","cancel":"cancelar","deleting":"Eliminando...","save":"Guardar cambios","saving":"Guardando...","saved":"¡Guardado!","upload":"Subir","uploading":"Subiendo...","uploading_filename":"Subiendo: %{filename}...","clipboard":"portapapeles","uploaded":"¡Subido!","pasting":"Pegando...","enable":"Activar","disable":"Desactivar","continue":"Continuar","undo":"Deshacer","revert":"Revertir","failed":"Falló","switch_to_anon":"Entrar en modo anónimo","switch_from_anon":"Salir del modo anónimo","banner":{"close":"Descartar este encabezado.","edit":"Editar este encabezado \u003e\u003e"},"pwa":{"install_banner":"¿Quieres \u003ca href\u003einstalar %{title} en este dispositivo?\u003c/a\u003e"},"choose_topic":{"none_found":"No se encontraron temas.","title":{"search":"Busca un tema","placeholder":"escribe el título, url o id del tema aquí"}},"choose_message":{"none_found":"No se han encontrado mensajes.","title":{"search":"Busca un mensaje","placeholder":"escribe el título, url o ID del mensaje aquí"}},"review":{"order_by":"Ordenar por","in_reply_to":"en respuesta a","explain":{"why":"explica por qué ha acabado en la cola","title":"Puntuación de revisable","formula":"Fórmula","subtotal":"Subtotal","total":"Total","min_score_visibility":"Puntuación mínima para ser visible","score_to_hide":"Puntuación mínima para ocultar publicación","take_action_bonus":{"name":"acción tomada","title":"Cuando un miembro del staff decide tomar acciones, se le otorga un bono al reporte."},"user_accuracy_bonus":{"name":"precisión del usuario","title":"Los usuarios con los que se ha coincidido en reportes anteriores reciben puntos extra."},"trust_level_bonus":{"name":"nivel de confianza","title":"Los revisables creados por usuarios con niveles de confianza elevados reciben una puntuación más alta."},"type_bonus":{"name":"tipo de bonificación","title":"Algunos tipos de revisables pueden recibir una bonificación por el staff para que tengan mayor prioridad."}},"claim_help":{"optional":"Puedes reclamar este artículo para evitar que otros lo revisen.","required":"Debes reclamar los artículos antes de poder revisarlos.","claimed_by_you":"Has reclamado este artículo y puedes revisarlo.","claimed_by_other":"Este artículo solo puede ser revisado por \u003cb\u003e%{username}\u003c/b\u003e."},"claim":{"title":"reclamar este tema"},"unclaim":{"help":"eliminar esta reclamación"},"awaiting_approval":"Esperando aprobación","delete":"Eliminar","settings":{"saved":"Guardado","save_changes":"Guardar cambios","title":"Ajustes","priorities":{"title":"Prioridades revisables"}},"moderation_history":"Historial de moderación","view_all":"Ver todo","grouped_by_topic":"Agrupado por tema","none":"No hay nada para revisar.","view_pending":"ver pendiente","topic_has_pending":{"one":"Este tema tiene \u003cb\u003e%{count}\u003c/b\u003e publicación esperando aprobación","other":"Este tema tiene \u003cb\u003e%{count}\u003c/b\u003e publicaciones esperando aprobación"},"title":"Revisión","topic":"Tema:","filtered_topic":"Has filtrado a contenido revisable en un solo tema.","filtered_user":"Usuario","filtered_reviewed_by":"Revisado por","show_all_topics":"mostrar todos los temas","deleted_post":"(publicación eliminada)","deleted_user":"(usuario eliminado)","user":{"bio":"Biografía","website":"Página web","username":"Nombre de usuario","email":"Correo electrónico","name":"Nombre","fields":"Campos","reject_reason":"Motivo"},"user_percentage":{"summary":{"one":"%{agreed}, %{disagreed}, %{ignored}, (del último reporte)","other":"%{agreed}, %{disagreed}, %{ignored} (de los últimos %{count} reportes)"},"agreed":{"one":"%{count}% de acuerdo","other":"%{count}% de acuerdo"},"disagreed":{"one":"%{count}% en desacuerdo","other":"%{count}% en desacuerdo"},"ignored":{"one":"%{count}% ignorado","other":"%{count}% ignorados"}},"topics":{"topic":"Tema","reviewable_count":"Cantidad","reported_by":"Reportado por","deleted":"[Tema eliminado]","original":"(tema original)","details":"detalles","unique_users":{"one":"%{count} usuario","other":"%{count} usuarios"}},"replies":{"one":"%{count} respuesta","other":"%{count} respuestas"},"edit":"Editar","save":"Guardar","cancel":"Cancelar","new_topic":"Aprobar este elemento creará un nuevo tema","filters":{"all_categories":"(todas las categorías)","type":{"title":"Tipo","all":"(todos los tipos)"},"minimum_score":"Puntuación mínima:","refresh":"Actualizar","status":"Estado","category":"Categoría","orders":{"score":"Puntuación","score_asc":"Puntuación (orden inverso)","created_at":"Creado el","created_at_asc":"Creado el (inverso)"},"priority":{"title":"Prioridad mínima","low":"(cualquiera)","medium":"Media","high":"Alta"}},"conversation":{"view_full":"ver conversación completa"},"scores":{"about":"Esta puntuación se calcula en función del nivel de confianza de quien reporta, la precisión de sus reportes anteriores y la prioridad del elemento que se reporta.","score":"Puntuación","date":"Fecha","type":"Tipo","status":"Estado","submitted_by":"Enviado por","reviewed_by":"Revisado por"},"statuses":{"pending":{"title":"Pendiente"},"approved":{"title":"Aprobado"},"rejected":{"title":"Rechazado"},"ignored":{"title":"Ignorado"},"deleted":{"title":"Eliminado"},"reviewed":{"title":"(todo revisado)"},"all":{"title":"(todo)"}},"types":{"reviewable_flagged_post":{"title":"Publicación reportada","flagged_by":"Reportada por"},"reviewable_queued_topic":{"title":"Tema en cola"},"reviewable_queued_post":{"title":"Publicación en cola"},"reviewable_user":{"title":"Usuario"}},"approval":{"title":"La publicación requiere aprobación","description":"Hemos recibido tu nueva publicación, pero debe ser aprobada por un moderador antes de que aparezca. Por favor, ten paciencia.","pending_posts":{"one":"Tienes \u003cstrong\u003e%{count}\u003c/strong\u003e publicación pendiente.","other":"Tienes \u003cstrong\u003e%{count}\u003c/strong\u003e publicaciones pendientes."},"ok":"OK"},"example_username":"nombre de usuario","reject_reason":{"title":"¿Por qué estás rechazando a este usuario?","send_email":"Enviar correo electrónico de rechazo"}},"time_shortcut":{"later_today":"Más tarde hoy","next_business_day":"Siguiente día hábil","tomorrow":"Mañana","next_week":"Próxima semana","post_local_date":"Fecha en la publicación","later_this_week":"Más tarde esta semana","start_of_next_business_week":"Lunes","start_of_next_business_week_alt":"El próximo lunes","next_month":"El próximo mes","custom":"Fecha y hora personalizadas","none":"No se necesita","last_custom":"Últimos"},"user_action":{"user_posted_topic":"\u003ca href='%{userUrl}'\u003e%{user}\u003c/a\u003e publicó \u003ca href='%{topicUrl}'\u003eel tema\u003c/a\u003e","you_posted_topic":"\u003ca href='%{userUrl}'\u003eTú\u003c/a\u003e publicaste \u003ca href='%{topicUrl}'\u003eel tema\u003c/a\u003e","user_replied_to_post":"\u003ca href='%{userUrl}'\u003e%{user}\u003c/a\u003e respondió a \u003ca href='%{postUrl}'\u003e%{post_number}\u003c/a\u003e","you_replied_to_post":"\u003ca href='%{userUrl}'\u003eTú\u003c/a\u003e respondiste a \u003ca href='%{postUrl}'\u003e%{post_number}\u003c/a\u003e","user_replied_to_topic":"\u003ca href='%{userUrl}'\u003e%{user}\u003c/a\u003e respondió en \u003ca href='%{topicUrl}'\u003eel tema\u003c/a\u003e","you_replied_to_topic":"\u003ca href='%{userUrl}'\u003eTú\u003c/a\u003e respondiste en \u003ca href='%{topicUrl}'\u003eel tema\u003c/a\u003e","user_mentioned_user":"\u003ca href='%{user1Url}'\u003e%{user}\u003c/a\u003e mencionó a \u003ca href='%{user2Url}'\u003e%{another_user}\u003c/a\u003e","user_mentioned_you":"\u003ca href='%{user1Url}'\u003e%{user}\u003c/a\u003e \u003ca href='%{user2Url}'\u003ete mencionó\u003c/a\u003e","you_mentioned_user":"\u003ca href='%{user1Url}'\u003eTú\u003c/a\u003e mencionaste a \u003ca href='%{user2Url}'\u003e%{another_user}\u003c/a\u003e","posted_by_user":"Publicado por \u003ca href='%{userUrl}'\u003e%{user}\u003c/a\u003e","posted_by_you":"Publicado por \u003ca href='%{userUrl}'\u003eti\u003c/a\u003e","sent_by_user":"Enviado por \u003ca href='%{userUrl}'\u003e%{user}\u003c/a\u003e","sent_by_you":"Enviado por \u003ca href='%{userUrl}'\u003eti\u003c/a\u003e"},"directory":{"username":"Nombre de usuario","filter_name":"filtrar por nombre de usuario","title":"Usuarios","likes_given":"Dados","likes_received":"Recibidos","topics_entered":"Vistos","topics_entered_long":"Temas vistos","time_read":"Tiempo leído","topic_count":"Temas","topic_count_long":"Temas creados","post_count":"Respuestas","post_count_long":"Respuestas publicadas","no_results":"No se han encontrado resultados.","days_visited":"Visitas","days_visited_long":"Días visitados","posts_read":"Leídos","posts_read_long":"Publicaciones leídas","last_updated":"Última actualización:","total_rows":{"one":"%{count} usuario","other":"%{count} usuarios"}},"group_histories":{"actions":{"change_group_setting":"Cambiar configuración de grupo","add_user_to_group":"Añadir usuario","remove_user_from_group":"Eliminar usuario","make_user_group_owner":"Hacer propietario","remove_user_as_group_owner":"Revocar propietario"}},"groups":{"member_added":"Añadido","member_requested":"Solicitado el","add_members":{"title":"Agregar miembros a %{group_name}","description":"También puedes pegar una lista separada por comas.","usernames":"Introduce nombres de usuario o direcciones de correo electrónico","input_placeholder":"Nombres de usuario o correos electrónicos","notify_users":"Notificar usuarios"},"requests":{"title":"Solicitudes","reason":"Motivo","accept":"Aceptar","accepted":"aceptado","deny":"Denegar","denied":"denegado","undone":"solicitud deshecha","handle":"atender solicitud de membresía"},"manage":{"title":"Gestionar","name":"Nombre","full_name":"Nombre completo","add_members":"Añadir miembros","delete_member_confirm":"¿Eliminar a «%{username}» del grupo «%{group}»?","profile":{"title":"Perfil"},"interaction":{"title":"Interacción","posting":"Publicando","notification":"Notificación"},"email":{"title":"Correo electrónico","status":"Sincronizados %{old_emails} / %{total_emails} correos a través de IMAP.","credentials":{"title":"Credenciales","smtp_server":"Servidor SMTP","smtp_port":"Puerto SMTP","smtp_ssl":"Usar SSL para SMTP","imap_server":"Servidor IMAP","imap_port":"Puerto IMAP","imap_ssl":"Usar SSL para IMAP","username":"Nombre de usuario","password":"Contraseña"},"settings":{"title":"Ajustes","allow_unknown_sender_topic_replies":"Permitir respuestas a temas de remitentes desconocidos.","allow_unknown_sender_topic_replies_hint":"Permite a remitentes desconocidos responder a temas de grupo. Si esto no está habilitado, las respuestas de direcciones de correo electrónico no incluidas en el hilo de correo electrónico IMAP o invitadas al tema crearán un nuevo tema."},"mailboxes":{"synchronized":"Bandeja de correo sincronizada","none_found":"No se han encontrado bandejas de correo para esta cuenta de correo electrónico.","disabled":"desactivado"}},"membership":{"title":"Membresía","access":"Acceso"},"categories":{"title":"Categorías","long_title":"Notificaciones predeterminadas de categoría","description":"Cuando se agregan usuarios a este grupo, la configuración de notificaciones de categorías será establecida a estos valores predeterminados. Después, pueden cambiarla.","watched_categories_instructions":"Vigilar automáticamente todos los temas en estas categorías. Los miembros del grupo serán notificados de cada nueva respuesta y tema, y un contador de respuestas nuevas al lado del tema.","tracked_categories_instructions":"Seguir automáticamente todos los temas en estas categorías. Aparecerá un contador de nuevas respuestas al lado del tema.","watching_first_post_categories_instructions":"Los usuarios serán notificados de la primera respuesta en cada tema nuevo en estas categorías.","regular_categories_instructions":"Si estas categorías están silenciadas, no estarán silenciadas para los miembros del grupo. Se les notificará a los usuarios si son mencionados o si alguien les responde.","muted_categories_instructions":"Los usuarios no serán notificados acerca de temas nuevos en estas categorías, y no aparecerán en las categorías o páginas de temas recientes."},"tags":{"title":"Etiquetas","long_title":"Ajustes de notificación por defecto para etiquetas","description":"Cuando se agregan usuarios a este grupo, su configuración de notificaciones de etiquetas será establecida a estos valores predeterminados. Después, pueden cambiarla.","watched_tags_instructions":"Vigilar automáticamente todos los temas con estas etiquetas. Los miembros del grupo serán notificados de todos los nuevos temas y respuestas, y un contados con el número de mensajes nuevos aparecerá junto al tema.","tracked_tags_instructions":"Seguir automáticamente todos los temas con estas etiquetas. Un contados con el número de mensajes nuevos aparecerá junto al tema.","watching_first_post_tags_instructions":"Se notificará a los usuarios cuando se publique un nuevo tema con estas etiquetas.","regular_tags_instructions":"Si estas etiquetas están silenciadas, dejarán de estarlo para los miembros del grupo. Los usuarios serán notificados si se les menciona o alguien les responde.","muted_tags_instructions":"No se notificará de nada relacionado con nuevos temas con estas etiquetas, y no aparecerán en recientes."},"logs":{"title":"Registros","when":"Cuándo","action":"Acción","acting_user":"Usuario accionante","target_user":"Usuario objetivo","subject":"Tema","details":"Detalles","from":"Desde","to":"Hasta"}},"permissions":{"title":"Permisos","none":"No hay categorías asociadas a este grupo.","description":"Los miembros de este grupo pueden acceder a estas categorías"},"public_admission":"Permitir que los usuarios se unan al grupo libremente (Se requiere que el grupo sea publicamente visible)","public_exit":"Permitir a los usuarios abandonar el grupo libremente","empty":{"posts":"No hay publicaciones por miembros de este grupo.","members":"Este grupo no tiene miembros.","requests":"No hay solicitudes de membresía para este grupo.","mentions":"No hay menciones de este grupo.","messages":"No hay mensajes para este grupo.","topics":"No hay temas por miembros de este grupo.","logs":"No hay registros para este grupo."},"add":"Añadir","join":"Unirse","leave":"Abandonar","request":"Solicitar","message":"Mensaje","confirm_leave":"¿Estás seguro de que quieres salir de este grupo?","allow_membership_requests":"Permitir a los usuarios enviar solicitudes de membresía a dueños de grupo (el grupo tiene que ser públicamente visible)","membership_request_template":"Plantilla personalizada que se muestra a los usuarios cuando envían una solicitud de membresía","membership_request":{"submit":"Enviar solicitud","title":"Solicitar unirse a @%{group_name}","reason":"Hazles saber a los propietarios del grupo por qué perteneces a este grupo"},"membership":"Membresía","name":"Nombre","group_name":"Nombre del grupo","user_count":"Usuarios","bio":"Acerca del grupo","selector_placeholder":"introduce tu nombre de usuario","owner":"propietario","index":{"title":"Grupos","all":"Todos los grupos","empty":"No hay grupos visibles.","filter":"Filtrar por tipo de grupo","owner_groups":"Grupos de los que soy propietario","close_groups":"Grupos cerrados","automatic_groups":"Grupos automáticos","automatic":"Automático","closed":"Cerrado","public":"Público","private":"Privado","public_groups":"Grupos públicos","automatic_group":"Grupo automático","close_group":"Cerrar grupo","my_groups":"Mis grupos","group_type":"Tipo de grupo","is_group_user":"Miembro","is_group_owner":"Propietario"},"title":{"one":"Grupo","other":"Grupos"},"activity":"Actividad","members":{"title":"Miembros","filter_placeholder_admin":"nombre de usuario o correo electrónico","filter_placeholder":"nombre de usuario","remove_member":"Eliminar miembro","remove_member_description":"Quitar a \u003cb\u003e%{username}\u003c/b\u003e de este grupo","make_owner":"Hacer propietario","make_owner_description":"Hacer a \u003cb\u003e%{username}\u003c/b\u003e un propietario de este grupo","remove_owner":"Eliminar como propietario","remove_owner_description":"Eliminar a \u003cb\u003e%{username}\u003c/b\u003e como propietario de este grupo","make_primary_description":"Hacer de este el grupo principal de \u003cb\u003e%{username}\u003c/b\u003e","remove_primary":"Eliminar como principal","remove_primary_description":"Eliminar esto como el grupo principal de \u003cb\u003e%{username}\u003c/b\u003e","remove_members":"Eliminar miembros","remove_members_description":"Eliminar a los usuarios seleccionados de este grupo","make_owners":"Hacer propietarios","make_owners_description":"Convertir a los usuarios seleccionados en propietarios de este grupo","remove_owners":"Eliminar propietarios","remove_owners_description":"Eliminar a los usuarios seleccionados como propietarios de este grupo","make_all_primary":"Hacer todo primario","owner":"Propietario","forbidden":"No tienes permitido ver los miembros del grupo."},"topics":"Temas","posts":"Publicaciones","mentions":"Menciones","messages":"Mensajes","notification_level":"Nivel de notificación predeterminado para mensajes de grupo","alias_levels":{"mentionable":"¿Quién puede @mencionar este grupo?","messageable":"¿Quién puede enviar mensajes a este grupo?","nobody":"Nadie","only_admins":"Solo administradores","mods_and_admins":"Solo moderadores y administradores","members_mods_and_admins":"Solo miembros del grupo, moderadores y administradores","owners_mods_and_admins":"Solo propietarios del grupo, moderadores y administradores","everyone":"Todos"},"notifications":{"watching":{"title":"Vigilando","description":"Se te notificará cada nueva publicación en cada mensaje y se mostrará un recuento de las respuestas nuevas."},"watching_first_post":{"title":"Vigilando la primera publicación","description":"Se te notificarán los mensajes nuevos en este grupo, pero no las respuestas a los mensajes."},"tracking":{"title":"Siguiendo","description":"Se te notificará si alguien menciona tu @nombre o te responde y se mostrará un recuento de las respuestas nuevas."},"regular":{"title":"Normal","description":"Se te notificará si alguien menciona tu @nombre o te responde."},"muted":{"title":"Silenciado","description":"No se te notificará nada sobre los mensajes en este grupo."}},"flair_url":"Imagen del sub-avatar","flair_upload_description":"Usa imágenes cuadradas de al menos 20px por 20px","flair_bg_color":"Color de fondo del sub-avatar","flair_bg_color_placeholder":"(Opcional) Valor de color hexadecimal","flair_color":"Color del sub-avatar","flair_color_placeholder":"(Opcional) Valor de color hexadecimal","flair_preview_icon":"Previsualización del icono","flair_preview_image":"Previsualización de la imagen","flair_type":{"icon":"Selecciona un icono","image":"Sube una imagen"}},"user_action_groups":{"1":"Me gusta dados","2":"Me gusta recibidos","3":"Marcadores","4":"Temas","5":"Respuestas","6":"Reacciones","7":"Menciones","9":"Citas","11":"Ediciones","12":"Elementos enviados","13":"Bandeja de entrada","14":"Pendiente","15":"Borradores"},"categories":{"all":"todas las categorías","all_subcategories":"todas","no_subcategory":"ninguna","category":"Categoría","category_list":"Mostrar lista de categorías","reorder":{"title":"Reordenar las categorías","title_long":"Reorganizar la lista de categorías","save":"Guardar orden","apply_all":"Aplicar","position":"Posición"},"posts":"Publicaciones","topics":"Temas","latest":"Recientes","toggle_ordering":"cambiar orden","subcategories":"Subcategorías","muted":"Categorías silenciadas","topic_sentence":{"one":"%{count} tema","other":"%{count} temas"},"topic_stat_unit":{"week":"semana","month":"mes"},"topic_stat_sentence_week":{"one":"%{count} nuevo tema en la última semana.","other":"%{count} temas nuevos en la última semana."},"topic_stat_sentence_month":{"one":"%{count} nuevo tema en el último mes.","other":"%{count} temas nuevos en el último mes."},"n_more":"Categorías (%{count} más)..."},"ip_lookup":{"title":"Búsqueda de dirección IP","hostname":"Nombre del host","location":"Ubicación","location_not_found":"(desconocido)","organisation":"Organización","phone":"Teléfono","other_accounts":"Otras cuentas con esta dirección IP:","delete_other_accounts":"Eliminar %{count}","username":"nombre de usuario","trust_level":"NC","read_time":"tiempo de lectura","topics_entered":"temas visitados","post_count":"nº. de publicaciones","confirm_delete_other_accounts":"¿Estás seguro de que quieres eliminar estas cuentas?","powered_by":"usando \u003ca href='https://maxmind.com'\u003eMaxMindDB\u003c/a\u003e","copied":"copiado"},"user_fields":{"none":"(selecciona una opción)","required":"Por favor, introduce un valor para \"%{name}\""},"user":{"said":"%{username}:","profile":"Perfil","mute":"Silenciar","edit":"Editar preferencias","download_archive":{"button_text":"Descargar todo","confirm":"¿Seguro de que quieres descargar tus publicaciones?","success":"Descarga iniciada, se te notificará por mensaje cuando el proceso se haya completado.","rate_limit_error":"Solo se pueden descargar las publicaciones una vez al día. Por favor, inténtalo de nuevo mañana."},"new_private_message":"Nuevo mensaje","private_message":"Mensaje","private_messages":"Mensajes","user_notifications":{"filters":{"filter_by":"Filtrar por","all":"Todo","read":"Leídos","unread":"Sin leer"},"ignore_duration_title":"Ignorar usuario","ignore_duration_username":"Nombre de usuario","ignore_duration_when":"Duración:","ignore_duration_save":"Ignorar","ignore_duration_note":"Por favor, ten en cuenta que todos los ignorados se eliminan automáticamente al expirar la duración especificada para esta acción.","ignore_duration_time_frame_required":"Por favor, selecciona un intervalo de tiempo","ignore_no_users":"No ignoras a ningún usuario","ignore_option":"Ignorado","ignore_option_title":"No recibirás notificaciones relacionadas con este usuario y todos sus temas y respuestas se ocultarán.","add_ignored_user":"Añadir...","mute_option":"Silenciado","mute_option_title":"No recibirás ninguna notificación relacionada con este usuario.","normal_option":"Normal","normal_option_title":"Se te notificará si este usuario te responde, cita o menciona."},"notification_schedule":{"title":"Horario de notificaciones","label":"Activar horario personalizado de notificaciones","tip":"Fuera de estas horas, se activará el modo «no molestar» automáticamente.","midnight":"Medianoche","none":"Ninguno","monday":"Lunes","tuesday":"Martes","wednesday":"Miércoles","thursday":"Jueves","friday":"Viernes","saturday":"Sábado","sunday":"Domingo","to":"a"},"activity_stream":"Actividad","read":"Leer","preferences":"Preferencias","feature_topic_on_profile":{"open_search":"Selecciona un nuevo tema","title":"Selecciona un tema","search_label":"Buscar tema por título","save":"Guardar","clear":{"title":"Quitar filtros","warning":"¿Estás seguro de que quieres eliminar tu tema destacado?"}},"use_current_timezone":"Usar zona horaria actual","profile_hidden":"El perfil público de este usuario está oculto.","expand_profile":"Expandir","collapse_profile":"Contraer","bookmarks":"Marcadores","bio":"Acerca de mí","timezone":"Zona horaria","invited_by":"Invitado por","trust_level":"Nivel de confianza","notifications":"Notificaciones","statistics":"Estadísticas","desktop_notifications":{"label":"Notificaciones en vivo","not_supported":"Las notificaciones no están disponibles en este navegador. Lo sentimos.","perm_default":"Activar notificaciones","perm_denied_btn":"Permiso denegado","perm_denied_expl":"Has denegado el permiso para las notificaciones. Configura tu navegador para permitir notificaciones. ","disable":"Desactivar notificaciones","enable":"Activar notificaciones","each_browser_note":"Nota: hay que cambiar este ajuste en cada navegador que uses. Todas las notificaciones serán desactivadas en el modo «no molestar», independientemente de este ajuste.","consent_prompt":"¿Quieres recibir notificaciones en vivo cuando alguien responda a tus mensajes?"},"dismiss":"Descartar","dismiss_notifications":"Descartar todo","dismiss_notifications_tooltip":"Marcar todas las notificaciones como leídas","first_notification":"¡Tu primera notificación! Selecciónala para empezar.","dynamic_favicon":"Mostrar número en el icono del navegador","skip_new_user_tips":{"description":"Omitir consejos de bienvenida y medallas","not_first_time":"¿No es tu primera vez?","skip_link":"Saltarse estos consejos"},"theme_default_on_all_devices":"Hacer que este sea el tema por defecto en todos mis dispositivos","color_scheme_default_on_all_devices":"Establecer la combinación de colores como predeterminada en todos mis dispositivos","color_scheme":"Combinación de colores","color_schemes":{"default_description":"Tema predeterminado","disable_dark_scheme":"Igual que regular","dark_instructions":"Puedes previsualizar el esquema de colores del modo oscuro activando o desactivando el modo oscuro de tu dispositivo.","undo":"Restablecer","regular":"Normal","dark":"Modo oscuro","default_dark_scheme":"(valor predeterminado por el sitio)"},"dark_mode":"Modo oscuro","dark_mode_enable":"Activar modo oscuro automáticamente","text_size_default_on_all_devices":"Hacer que este sea el tamaño por defecto en todos mis dispositivos","allow_private_messages":"Permitir que otros usuarios me envíen mensajes privados","external_links_in_new_tab":"Abrir todos los enlaces externos en una nueva pestaña","enable_quoting":"Activar respuesta citando el texto seleccionado","enable_defer":"Activar diferir para marcar temas no leídos","change":"cambio","featured_topic":"Tema destacado","moderator":"%{user} es un moderador","admin":"%{user} es un administrador","moderator_tooltip":"Este usuario es un moderador","admin_tooltip":"Este usuario es un administrador","silenced_tooltip":"Este usuario está silenciado","suspended_notice":"Este usuario ha sido suspendido hasta %{date}.","suspended_permanently":"Este usuario está suspendido.","suspended_reason":"Motivo: ","github_profile":"GitHub","email_activity_summary":"Resumen de actividad","mailing_list_mode":{"label":"Modo lista de correo","enabled":"Activar modo lista de correo","instructions":"Esta opción sobrescribe el resumen de actividad.\u003cbr /\u003e\nLos temas y categorías silenciados no se incluyen en estos correos electrónicos.\n","individual":"Enviar un correo electrónico por cada publicación nueva","individual_no_echo":"Enviar un correo electrónico por cada publicación nueva excepto aquellas publicadas por mí","many_per_day":"Envíame un correo electrónico por cada publicación nueva (unos %{dailyEmailEstimate} por día)","few_per_day":"Envíame un correo electrónico por cada publicación nueva (unos 2 por día)","warning":"Modo de lista de correo habilitado. La configuración de notificaciones por correo electrónico está anulada."},"tag_settings":"Etiquetas","watched_tags":"Vigiladas","watched_tags_instructions":"Vigilarás automáticamente todos los temas con estas etiquetas. Se te notificarán todas las publicaciones y temas nuevos y aparecerá un contador de publicaciones nuevas al lado del tema.","tracked_tags":"Siguiendo","tracked_tags_instructions":"Seguirás automáticamente todos los temas con estas etiquetas. Aparecerá un contador de publicaciones nuevas al lado del tema.","muted_tags":"Silenciadas","muted_tags_instructions":"No recibirás notificaciones de ningún tema con estas etiquetas y estas no aparecerán en la pestaña Recientes.","watched_categories":"Vigiladas","watched_categories_instructions":"Vigilarás automáticamente todos los temas en estas categorías. Se te notificarán todos las publicaciones y temas nuevos y aparecerá un contador de publicaciones nuevas al lado del tema.","tracked_categories":"Siguiendo","tracked_categories_instructions":"Seguirás automáticamente todos los temas en estas categorías. Aparecerá un contador de publicaciones nuevas al lado del tema.","watched_first_post_categories":"Vigilar la primera publicación","watched_first_post_categories_instructions":"Se te notificará la primera publicación en cada tema nuevo en estas categorías.","watched_first_post_tags":"Vigilando la primera publicación","watched_first_post_tags_instructions":"Se te notificará la primera publicación en cada tema nuevo con estas etiquetas.","muted_categories":"Silenciado","muted_categories_instructions":"No se te notificará acerca de ningún tema en estas categorías y no aparecerán en la página de categorías o mensajes recientes.","muted_categories_instructions_dont_hide":"No se te notificará nada acerca de temas nuevos en estas categorías.","regular_categories":"Habitual","regular_categories_instructions":"Verás estas categorías en las listas de temas «Recientes» y «Destacados».","no_category_access":"Como moderador tienes acceso limitado a categorías. Guardar está deshabilitado.","delete_account":"Eliminar mi cuenta","delete_account_confirm":"¿Estás seguro de que quieres eliminar permanentemente tu cuenta? ¡Esta acción no puede ser revertida!","deleted_yourself":"Tu cuenta se ha eliminado exitosamente.","delete_yourself_not_allowed":"Por favor, contacta a un miembro del staff si deseas que se elimine tu cuenta.","unread_message_count":"Mensajes","admin_delete":"Eliminar","users":"Usuarios","muted_users":"Silenciados","muted_users_instructions":"Omitir todas las notificaciones y mensajes personales de estos usuarios.","allowed_pm_users":"Permitidos","allowed_pm_users_instructions":"Solo permitir mensajes personales de estos usuarios.","allow_private_messages_from_specific_users":"Solo permitir que ciertos usuarios que me envíen mensajes personales.","ignored_users":"Ignorados","ignored_users_instructions":"Omitir todas las publicaciones, notificaciones y mensajes personales de estos usuarios.","tracked_topics_link":"Mostrar","automatically_unpin_topics":"Desfijar temas automáticamente cuando los leo por completo.","apps":"Aplicaciones","revoke_access":"Revocar acceso","undo_revoke_access":"Deshacer revocación de acceso","api_approved":"Fecha de aprobación:","api_last_used_at":"Fecha de último uso:","theme":"Tema","save_to_change_theme":"Este tema se actualizará después de que pulses \"%{save_text}\"","home":"Página de inicio por defecto","staged":"Temporal","staff_counters":{"flags_given":"reportes útiles","flagged_posts":"publicaciones reportadas","deleted_posts":"publicaciones eliminadas","suspensions":"suspensiones","warnings_received":"avisos","rejected_posts":"publicaciones rechazadas"},"messages":{"all":"Todos","inbox":"Bandeja de entrada","sent":"Enviados","archive":"Archivo","groups":"Mis grupos","bulk_select":"Mensajes seleccionados","move_to_inbox":"Mover a la bandeja de entrada","move_to_archive":"Archivar","failed_to_move":"No se han podido mover los mensajes seleccionados (podrías estar teniendo problemas de conexión)","select_all":"Seleccionar todo","tags":"Etiquetas"},"preferences_nav":{"account":"Cuenta","profile":"Perfil","emails":"Correos electrónicos","notifications":"Notificaciones","categories":"Categorías","users":"Usuarios","tags":"Etiquetas","interface":"Interfaz","apps":"Aplicaciones"},"change_password":{"success":"(correo electrónico enviado)","in_progress":"(enviando correo electrónico)","error":"(error)","emoji":"emoji candado","action":"Enviar correo electrónico para restablecer la contraseña","set_password":"Establecer contraseña","choose_new":"Escoge una nueva contraseña","choose":"Escoge una contraseña"},"second_factor_backup":{"title":"Códigos de respaldo de la verificación en dos factores","regenerate":"Generar nuevos","disable":"Desactivar","enable":"Activar","enable_long":"Habilitar códigos de respaldo","manage":{"one":"Gestiona tus códigos de respaldo. Te queda \u003cstrong\u003e%{count}\u003c/strong\u003e código sin usar.","other":"Gestiona tus códigos de respaldo. Te quedan \u003cstrong\u003e%{count}\u003c/strong\u003e códigos sin usar."},"copy_to_clipboard":"Copiar al portapapeles","copy_to_clipboard_error":"Error al copiar datos al portapapeles","copied_to_clipboard":"Copiado al portapapeles","download_backup_codes":"Descargar códigos de recuperación","remaining_codes":{"one":"Te queda \u003cstrong\u003e%{count}\u003c/strong\u003e código de respaldo sin usar todavía.","other":"Te quedan \u003cstrong\u003e%{count}\u003c/strong\u003e códigos de respaldo sin usar todavía."},"use":"Usar un código de respaldo","enable_prerequisites":"Debes activar un método de segundo factor antes de generar códigos de respaldo.","codes":{"title":"Códigos de respaldo generados","description":"Cada uno de estos códigos de respaldo puede ser usado una única vez. Manténlos en un lugar seguro pero accesible."}},"second_factor":{"title":"Autenticación de dos factores","enable":"Gestionar la autenticación de dos factores","disable_all":"Desactivar todo","forgot_password":"¿Has olvidado tu contraseña?","confirm_password_description":"Por favor, confirma tu contraseña para continuar","name":"Nombre","label":"Código","rate_limit":"Por favor, espera antes de intentar utilizar otro código de autenticación.","enable_description":"Escanea este código QR en una aplicación que lo soporte (\u003ca href=\"https://www.google.com/search?q=authenticator+apps+for+android\" target=\"_blank\"\u003eAndroid\u003c/a\u003e – \u003ca href=\"https://www.google.com/search?q=authenticator+apps+for+ios\" target=\"_blank\"\u003eiOS\u003c/a\u003e) e introduce el código de autenticación.\n","disable_description":"Por favor, ingresa el código de autenticación que aparece en tu aplicación","show_key_description":"Introducir manualmente","short_description":"Protege tu cuenta mediante códigos de respaldo de un solo uso.\n","extended_description":"La autenticación de dos factores añade una capa extra de seguridad a tu cuenta al pedirte un código de un solo uso además de tu contraseña. Los códigos se pueden generar en dispositivos \u003ca href=\"https://www.google.com/search?q=authenticator+apps+for+android\" target='_blank'\u003eAndroid\u003c/a\u003e e \u003ca href=\"https://www.google.com/search?q=authenticator+apps+for+ios\"\u003eiOS\u003c/a\u003e.\n","oauth_enabled_warning":"Ten en cuenta que los inicios de sesión con redes sociales y páginas de terceros se desactivarán para tu cuenta una vez que actives la autenticación de dos factores.","use":"Usar app Authenticator","enforced_notice":"Es obligatorio que actives la autenticación de dos factores antes de acceder al sitio web.","disable":"Inhabilitar","disable_confirm":"¿Seguro que quieres desactivar todos los métodos de segundo factor?","save":"Guardar","edit":"Editar","edit_title":"Editar autenticador","edit_description":"Nombre del autenticador","enable_security_key_description":"Cuando tengas tu \u003ca href=\"https://www.google.com/search?q=hardware+security+key\" target=\"_blank\"\u003ellave de seguridad\u003c/a\u003e preparada, pulsa el botón de Registrar más abajo.\n","totp":{"title":"Autenticación basada en tokens","add":"Añadir autentificador","default_name":"Mi autenticador","name_and_code_required_error":"Debes introducir un nombre y un código de tu aplicación de autentificación."},"security_key":{"register":"Registrar","title":"Claves de seguridad","add":"Añadir clave de seguridad","default_name":"Clave de seguridad principal","not_allowed_error":"El proceso de registro de clave de seguridad fue cancelado o se agotó el tiempo.","already_added_error":"Ya registraste esta clave de seguridad. No tienes que registrarla de nuevo.","edit":"Editar clave de seguridad","save":"Guardar","edit_description":"Nombre de la clave de seguridad","name_required_error":"Debes introducir un nombre para tu llave de seguridad."}},"change_about":{"title":"Cambiar «Acerca de mí»","error":"Ha ocurrido un error al cambiar este valor."},"change_username":{"title":"Cambiar nombre de usuario","confirm":"¿Estás completamente seguro de que quieres cambiar tu nombre de usuario?","taken":"Lo sentimos, ese nombre de usuario ya se encuentra en uso.","invalid":"Este nombre de usuario no es válido. Este solo puede incluir números y letras"},"add_email":{"title":"Añadir correo electrónico","add":"añadir"},"change_email":{"title":"Cambiar correo electrónico","taken":"Lo sentimos, ese correo electrónico no está disponible.","error":"Ha ocurrido un error al cambiar tu correo electrónico. ¿Tal vez esa dirección ya se encuentra en uso?","success":"Te hemos enviado un correo electrónico a esa dirección. Por favor, sigue las instrucciones de confirmación.","success_via_admin":"Te hemos enviado un correo electrónico a esa dirección. Por favor, sigue las instrucciones de confirmación.","success_staff":"Hemos enviado un correo electrónico a tu dirección actual. Por favor, sigue las instrucciones de confirmación."},"change_avatar":{"title":"Cambiar tu imagen de perfil","gravatar":"\u003ca href='//%{gravatarBaseUrl}%{gravatarLoginUrl}' target='_blank'\u003e%{gravatarName}\u003c/a\u003e, basado en","gravatar_title":"Cambia tu avatar en la pagina web de %{gravatarName}","gravatar_failed":"No pudimos encontrar un %{gravatarName} con esa dirección de correo electrónico.","refresh_gravatar_title":"Actualiza tu %{gravatarName}","letter_based":"Imagen de perfil asignada por el sistema","uploaded_avatar":"Foto personalizada","uploaded_avatar_empty":"Agrega una foto personalizada","upload_title":"Sube tu foto","image_is_not_a_square":"Advertencia: hemos recortado tu imagen porque la anchura y la altura no eran iguales."},"change_profile_background":{"title":"Encabezado de perfil","instructions":"Por defecto, los encabezados de perfil estarán centrados y tendrán una anchura de 1110px."},"change_card_background":{"title":"Fondo de tarjeta de usuario","instructions":"Las imágenes de fonodo estarán centradas y tendrán una anchura predeterminada de 590 px."},"change_featured_topic":{"title":"Tema destacado","instructions":"Un enlace a este tema estará en tu tarjeta de usuario y perfil."},"email":{"title":"Correo electrónico","primary":"Correo electrónico principal","secondary":"Correos electrónicos secundarios","primary_label":"primario","unconfirmed_label":"sin confirmar","resend_label":"reenviar correo de confirmación","resending_label":"enviando...","resent_label":"correo enviado","update_email":"Cambiar correo electrónico","set_primary":"Establecer como correo primario","destroy":"Quitar correo","add_email":"Añadir correo alternativo","auth_override_instructions":"El correo electrónico se puede actualizar desde el proveedor de autenticación.","no_secondary":"Sin correos electrónicos secundarios","instructions":"Nunca se mostrará al público.","admin_note":"Nota: Un usuario administrador que cambia el correo electrónico de otro usuario no administrador indica que el usuario ha perdido el acceso a su cuenta de correo electrónico original, por lo que se enviará un correo electrónico para restablecer la contraseña a su nueva dirección. El correo electrónico del usuario no cambiará hasta que complete el proceso de restablecimiento de contraseña.","ok":"Te enviaremos un correo electrónico para confirmar","required":"Por favor, introduce un correo electrónico","invalid":"Por favor, ingresa una dirección de correo electrónico válida","authenticated":"Tu  correo electrónico ha sido autenticado por %{provider}","frequency_immediately":"Te enviaremos un correo electrónico inmediatamente si no has leído el asunto por el cual te estamos enviando el correo.","frequency":{"one":"Sólo te enviaremos emails si no te hemos visto en el último minuto.","other":"Solo te enviaremos un correo electrónico si no te hemos visto en los últimos %{count} minutos."}},"associated_accounts":{"title":"Cuentas asociadas","connect":"Conectar","revoke":"Revocar","cancel":"Cancelar","not_connected":"(no conectada)","confirm_modal_title":"Conectar cuenta de %{provider}","confirm_description":{"account_specific":"Tu cuenta de %{provider} «%{account_description}» se utilizará para la autenticación.","generic":"Tu cuenta de %{provider} se utilizará para la autenticación."}},"name":{"title":"Nombre","instructions":"tu nombre completo (opcional)","instructions_required":"Tu nombre completo","required":"Por favor, introduce un numbre","too_short":"Tu nombre es demasiado corto","ok":"Tu nombre se ve adecuado"},"username":{"title":"Nombre de usuario","instructions":"único, sin espacios y corto","short_instructions":"Los demás usuarios pueden mencionarte como @%{username}","available":"Tu nombre de usuario está disponible","not_available":"No disponible. ¿Deseas intentar %{suggestion}?","not_available_no_suggestion":"No disponible","too_short":"Tu nombre de usuario es demasiado corto","too_long":"Tu nombre de usuario es demasiado largo","checking":"Comprobando la disponibilidad del nombre de usuario...","prefilled":"El correo electrónico coincide con el nombre de usuario registrado","required":"Por favor, introduce un nombre de usuari"},"locale":{"title":"Idioma de la interfaz","instructions":"Idioma de la interfaz. Cambiará cuando recargues la página.","default":"(por defecto)","any":"cualquiera"},"password_confirmation":{"title":"Ingresa de nuevo la contraseña"},"invite_code":{"title":"Código de invitación","instructions":"El registro de la cuenta requiere un código de invitación"},"auth_tokens":{"title":"Dispositivos utilizados recientemente","details":"Detalles","log_out_all":"Cerrar sesión en todos los dispositivos","not_you":"¿No eres tú?","show_all":"Mostrar todos (%{count})","show_few":"Mostrar menos","was_this_you":"¿Fuiste tú?","was_this_you_description":"Si no fuiste tú, te recomendamos que cambies tu contraseña y cierres sesión en todos los dispositivos.","browser_and_device":"%{browser} en %{device}","secure_account":"Asegurar mi cuenta","latest_post":"Publicaste por última vez...","device_location":"\u003cspan class=\"auth-token-device\"\u003e%{device}\u003c/span\u003e \u0026ndash; \u003cspan title=\"IP: %{ip}\"\u003e%{location}\u003c/span\u003e","browser_active":"%{browser} | \u003cspan class=\"active\"\u003eactivo ahora mismo\u003c/span\u003e","browser_last_seen":"%{browser} | %{date}"},"last_posted":"Última publicación","last_emailed":"Último correo electrónico enviado","last_seen":"Visto por última vez","created":"Creado el","log_out":"Cerrar sesión","location":"Ubicación","website":"Sitio web","email_settings":"Correo electrónico","hide_profile_and_presence":"Ocultar mi perfil público y elementos de presencia","enable_physical_keyboard":"Activar soporte de teclado físico en iPad","text_size":{"title":"Tamaño del texto","smallest":"Más pequeño","smaller":"Pequeño","normal":"Normal","larger":"Grande","largest":"Muy grande"},"title_count_mode":{"title":"El título de la página muestra una cuenta de:","notifications":"Notificaciones nuevas","contextual":"Contenido nuevo en la página"},"like_notification_frequency":{"title":"Notificar cuando me dan me gusta","always":"Siempre","first_time_and_daily":"Cuando mi publicación reciba el primer me gusta y luego diariamente si recibe más","first_time":"Cuando mi publicación reciba el primer me gusta","never":"Nunca"},"email_previous_replies":{"title":"Incluir respuestas previas en la parte inferior de los correos electrónicos","unless_emailed":"a menos que se hayan enviado previamente","always":"siempre","never":"nunca"},"email_digests":{"title":"Cuando no visite el sitio, enviarme un correo electrónico con un resumen de los temas y respuestas populares","every_30_minutes":"cada 30 minutos","every_hour":"cada hora","daily":"diariamente","weekly":"semanalmente","every_month":"cada mes","every_six_months":"cada seis meses"},"email_level":{"title":"Enviarme un correo electrónico cuando alguien me cite, me responda, mencione mi @nombre de usuario o me invite a un tema","always":"siempre","only_when_away":"solo cuando no esté en la página","never":"nunca"},"email_messages_level":"Enviarme un correo electrónico cuando alguien me mande un mensaje","include_tl0_in_digests":"Incluir contenido de usuarios nuevos en los correos electrónicos de resumen","email_in_reply_to":"Incluir un extracto de la publicación que recibió una respuesta en los correo electrónicos","other_settings":"Otros","categories_settings":"Categorías","new_topic_duration":{"label":"Considerar que los temas son nuevos cuando","not_viewed":"No los he visto todavía","last_here":"creados desde mi última visita","after_1_day":"creados durante el último día ","after_2_days":"creados durante los últimos 2 días","after_1_week":"creados durante la última semana","after_2_weeks":"creados durante las últimas 2 semanas"},"auto_track_topics":"Seguir automáticamente temas en los que entre","auto_track_options":{"never":"nunca","immediately":"inmediatamente","after_30_seconds":"después de 30 segundos","after_1_minute":"después de 1 minuto","after_2_minutes":"después de 2 minutos","after_3_minutes":"después de 3 minutos","after_4_minutes":"después de 4 minutos","after_5_minutes":"después de 5 minutos","after_10_minutes":"después de 10 minutos"},"notification_level_when_replying":"Cuando publique en un tema, cambia el nivel de seguimiento a","invited":{"search":"escribe para buscar invitaciones...","title":"Invitaciones","user":"Usuario invitado","sent":"Última vez enviada","none":"Sin invitaciones para mostrar.","truncated":{"one":"Mostrando la primera invitación.","other":"Mostrando las primeras %{count} invitaciones."},"redeemed":"Invitaciones aceptadas","redeemed_tab":"Aceptada","redeemed_tab_with_count":"Aceptadas (%{count})","redeemed_at":"Aceptada","pending":"Invitaciones pendientes","pending_tab":"Pendiente","pending_tab_with_count":"Pendientes (%{count})","topics_entered":"Temas vistos","posts_read_count":"Publicaciones leídas","expired":"Esta invitación ha caducado.","rescind":"Eliminar","rescinded":"Invitación eliminada","rescind_all":"Eliminar invitaciones caducadas","rescinded_all":"¡Todas las invitaciones expiradas se han eliminado!","rescind_all_confirm":"¿Estás seguro de querer eliminar todas las invitaciones expiradas?","reinvite":"Reenviar Invitación","reinvite_all":"Reenviar todas las invitaciones","reinvite_all_confirm":"¿Estás seguro de que quieres reenviar todas las invitaciones?","reinvited":"Invitación reenviada","reinvited_all":"¡Todas las invitaciones se han reenviado!","time_read":"Tiempo de lectura","days_visited":"Días visitados","account_age_days":"Antigüedad de la cuenta en días","source":"Invitado a través de","links_tab":"Enlaces","links_tab_with_count":"Enlaces (%{count})","link_url":"Enlace","link_created_at":"Creado","link_redemption_stats":"Canjes","link_groups":"Grupos","link_expires_at":"Caducidad","create":"Invitar","copy_link":"Mostrar enlace","generate_link":"Crear enlacew de invitación","link_generated":"Aquí tienes el enlace","valid_for":"El enlace de invitación solo es válido para esta dirección de correo electrónico: %{email}","single_user":"Invitar a través de correo electrónico","multiple_user":"Invitar a través de enlace","invite_link":{"title":"Enlace de invitación","success":"¡Enlace de invitación generado satisfactoriamente!","error":"Ha ocurrido un error al generar el enlace de invitación","max_redemptions_allowed_label":"¿Cuánta gente puede registrarse con este enlace?","expires_at":"¿Cuándo caducará este enlace?"},"bulk_invite":{"none":"No hay invitaciones para mostrar en esta página.","text":"Invitaciones masivas","success":"Archivo subido correctamente, se te notificará mediante un mensaje cuando se complete el proceso.","error":"Lo sentimos, el formato del archivo debe ser CSV. ","confirmation_message":"Estás a punto de enviar invitaciones por correo electrónico a todas las direcciones en el archivo subido."}},"password":{"title":"Contraseña","too_short":"Tu contraseña es demasiada corta.","common":"Esta contraseña es demasiado común.","same_as_username":"Tu contraseña es la misma que tu nombre de usuario.","same_as_email":"Tu contraseña es la misma que tu correo electrónico.","ok":"Tu contraseña es válida.","instructions":"debe tener al menos %{count} caracteres","required":"Por favor, introduce una contraseña"},"summary":{"title":"Resumen","stats":"Estadísticas","time_read":"tiempo de lectura","recent_time_read":"tiempo de lectura reciente","topic_count":{"one":"tema creado","other":"temas creados"},"post_count":{"one":"publicación creada","other":"publicaciones creadas"},"likes_given":{"one":"dado","other":"dados"},"likes_received":{"one":"recibido","other":"recibidos"},"days_visited":{"one":"día visitado","other":"días visitados"},"topics_entered":{"one":"tema visto","other":"temas vistos"},"posts_read":{"one":"publicación leída","other":"publicaciones leídas"},"bookmark_count":{"one":"marcador","other":"marcadores"},"top_replies":"Respuestas destacadas","no_replies":"No hay respuestas aún.","more_replies":"Más respuestas","top_topics":"Temas destacados","no_topics":"No hay temas aún.","more_topics":"Más temas","top_badges":"Medallas destacadas","no_badges":"Todavía no hay medallas.","more_badges":"Más medallas","top_links":"Enlaces destacados","no_links":"No hay enlaces aún.","most_liked_by":"Recibió mas me gusta de","most_liked_users":"Dio más me gusta a","most_replied_to_users":"Respondió más a","no_likes":"No hay ningún me gusta aún.","top_categories":"Categorías destacadas","topics":"Temas","replies":"Respuestas"},"ip_address":{"title":"Última dirección IP"},"registration_ip_address":{"title":"Dirección IP del registro"},"avatar":{"title":"Imagen de perfil","header_title":"perfil, mensajes, marcadores y preferencias"},"title":{"title":"Título","none":"(ninguno)"},"primary_group":{"title":"Grupo principal","none":"(ninguno)"},"filters":{"all":"Todos"},"stream":{"posted_by":"Publicado por","sent_by":"Enviado por","private_message":"mensaje","the_topic":"el tema"}},"loading":"Cargando...","errors":{"prev_page":"mientras se intentaba cargar","reasons":{"network":"Error de red","server":"Error del servidor","forbidden":"Acceso denegado","unknown":"Error","not_found":"Página no encontrada"},"desc":{"network":"Por favor, revisa tu conexión.","network_fixed":"Parece que ha vuelto.","server":"Código de error: %{status}","forbidden":"No tienes permitido ver esto.","not_found":"¡Ups! La aplicación intentó cargar una URL inexistente.","unknown":"Algo salió mal."},"buttons":{"back":"Volver atrás","again":"Intentar de nuevo","fixed":"Cargar página"}},"modal":{"close":"cerrar","dismiss_error":"Descartar error"},"close":"Cerrar","assets_changed_confirm":"Este sitio acaba de ser actualizado. ¿Quieres cargar la página de nuevo para ver la última versión?","logout":"Has cerrado sesión.","refresh":"Actualizar","home":"Inicio","read_only_mode":{"enabled":"Este sitio está en modo de solo lectura. Puedes continuar navegando pero algunas acciones como responder o dar me gusta no están disponibles por ahora.","login_disabled":"Iniciar sesión está desactivado mientras el foro se encuentre en modo de solo lectura.","logout_disabled":"Cerrar sesión está desactivado mientras el sitio se encuentre en modo de solo lectura."},"logs_error_rate_notice":{},"learn_more":"saber más...","first_post":"Primera publicación","mute":"Silenciar","unmute":"No silenciar","last_post":"Publicado","local_time":"Hora local","time_read":"Leído","time_read_recently":"%{time_read} recientemente","time_read_tooltip":"%{time_read} tiempo de lectura total","time_read_recently_tooltip":"%{time_read} tiempo de lectura total (%{recent_time_read} en los últimos 60 días)","last_reply_lowercase":"última respuesta","replies_lowercase":{"one":"respuesta","other":"respuestas"},"signup_cta":{"sign_up":"Registrarse","hide_session":"Recúerdame mañana","hide_forever":"no, gracias","hidden_for_session":"Vale, te preguntaremos mañana. Recuerda que también puedes usar el botón «Iniciar sesión» para crear una cuenta en cualquier momento.","intro":"¡Hola! Parece que estás disfrutando la discusión, pero no has creado una cuenta todavía.","value_prop":"Cuando creas una cuenta, recordamos exactamente lo que has leído de modo que puedas retomar lo que leías justo donde lo dejaste. También recibes notificaciones, por aquí y por correo electrónico, cuando alguien responde a tus mensajes. ¡También puedes darle «me gusta» a los mensajes para compartir amor! :heartpulse:"},"summary":{"enabled_description":"Estás viendo un resumen de este tema: las publicaciones más interesantes de acuerdo a la comunidad.","enable":"Resumir este tema","disable":"Ver todas las publicaciones"},"deleted_filter":{"enabled_description":"Este tema contiene publicaciones eliminadas que se han ocultado.","disabled_description":"Se muestran las publicaciones eliminadas de este tema. ","enable":"Ocultar publicaciones eliminadas","disable":"Mostrar publicaciones eliminadas"},"private_message_info":{"title":"Mensaje","invite":"Invitar a más gente...","edit":"Añadir o quitar...","remove":"Quitar...","add":"Añadir...","leave_message":"¿Estás seguro de que quieres abandonar este mensaje?","remove_allowed_user":"¿Estás seguro de que quieres eliminar a %{name} de este mensaje?","remove_allowed_group":"¿Estás seguro de que quieres eliminar a %{name} de este mensaje?"},"email":"Correo electrónico","username":"Nombre de usuario","last_seen":"Visto por última vez","created":"Creado","created_lowercase":"creado","trust_level":"Nivel de confianza","search_hint":"usuario, correo electrónico o dirección IP","create_account":{"header_title":"¡Bienvenido/a!","subheader_title":"Vamos a crear tu cuenta","disclaimer":"Al registrarte aceptas la \u003ca href='%{privacy_link}' target='blank'\u003ePolítica de privacidad\u003c/a\u003e y los \u003ca href='%{tos_link}' target='blank'\u003eTérminos de servicio\u003c/a\u003e.","title":"Crea tu cuenta","failed":"Algo salió mal. Quizás este correo electrónico ya se encuentra registrado. Intenta con el enlace «olvidé la contraseña»"},"forgot_password":{"title":"Restablecer contraseña","action":"Olvidé mi contraseña","invite":"Ingresa tu nombre de usuario o tu dirección de correo electrónico, y te enviaremos un correo electrónico para restablecer tu contraseña.","reset":"Restablecer contraseña","complete_username":"Si una cuenta coincide con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e, en breve deberías recibir un correo electrónico con las instrucciones para restablecer tu contraseña.","complete_email":"Si una cuenta coincide con \u003cb\u003e%{email}\u003c/b\u003e, en breve deberías recibir un correo electrónico con las instrucciones para restablecer tu contraseña.","complete_username_found":"Encontramos una cuenta que coincide con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e. Debes recibir pronto un correo electrónico con instrucciones para restablecer tu contraseña.","complete_email_found":"Encontramos una cuenta que coincide con el correo electrónico \u003cb\u003e%{email}\u003c/b\u003e. Debes recibir pronto un correo electrónico con instrucciones para restablecer tu contraseña.","complete_username_not_found":"No hay ninguna cuenta que coincida con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e","complete_email_not_found":"No hay ninguna cuenta que coincida con el correo electrónico \u003cb\u003e%{email}\u003c/b\u003e","help":"¿No te ha llegado el correo? Asegúrate de comprobar primero tu carpeta de correo no deseado. \u003cp\u003e¿No estás seguro de qué correo has usado? Ingresa tu correo electrónico y te avisaremos si lo tenemos registrado.\u003c/p\u003e\u003cp\u003eSi no tienes acceso al correo electrónico asociado a tu cuenta, por favor, contacta \u003ca href='%{basePath}/about'\u003ea nuestro amable staff.\u003c/a\u003e\u003c/p\u003e","button_ok":"OK","button_help":"Ayuda"},"email_login":{"link_label":"Enviarme un enlace para ingresar por correo electrónico","button_label":"con correo electrónico","emoji":"emoji de candado","complete_username":"Si una cuenta coincide con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e, en breve deberías recibir un correo electrónico con un enlace para ingresar a tu cuenta.","complete_email":"Si una cuenta coincide con \u003cb\u003e%{email}\u003c/b\u003e, en breve deberías recibir un correo electrónico con un enlace para ingresar a tu cuenta.","complete_username_found":"Encontramos una cuenta que coincide con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e, en breve deberías recibir un correo electrónico con un enlace para ingresar a tu cuenta.","complete_email_found":"Encontramos una cuenta que coincide con \u003cb\u003e%{email}\u003c/b\u003e, deberías recibir un email con un enlace de ingreso en breve.","complete_username_not_found":"No hay ninguna cuenta que coincida con el nombre de usuario \u003cb\u003e%{username}\u003c/b\u003e","complete_email_not_found":"No hay ninguna cuenta que coincida con el correo electrónico \u003cb\u003e%{email}\u003c/b\u003e","confirm_title":"Continuar a %{site_name}","logging_in_as":"Iniciando sesión como %{email}","confirm_button":"Finalizar inicio de sesión"},"login":{"subheader_title":"Inicie sesión en su cuenta","title":"Iniciar sesión","username":"Usuario","password":"Contraseña","second_factor_title":"Autenticación de dos factores","second_factor_description":"Por favor, ingresa el código de autenticación desde tu aplicación:","second_factor_backup":"Iniciar sesión utilizando un código de respaldo","second_factor_backup_title":"Código de respaldo de la autenticación de dos factores","second_factor_backup_description":"Por favor, ingresa uno de los códigos de respaldo:","second_factor":"Iniciar sesión utilizando la app Authenticator","security_key_description":"Cuando tengas tu clave de seguridad física preparada, presiona el botón de autenticar con clave de seguridad que se encuentra debajo.","security_key_alternative":"Intenta de otra manera","security_key_authenticate":"Autenticar con clave de seguridad","security_key_not_allowed_error":"La autenticación de la clave de seguridad fue cancelada o se agotó el tiempo.","security_key_no_matching_credential_error":"No se encontraron credenciales que coincidan en la clave de seguridad provista.","security_key_support_missing_error":"Tu dispositivo o navegador actual no soporta el uso de claves de seguridad. Por favor, utiliza un método diferente.","email_placeholder":"Correo electrónico / Nombre de Usuario","caps_lock_warning":"El bloqueo de mayúsculas está activado","error":"Error desconocido","cookies_error":"Parece que tu navegador tiene deshabilitados los cookies. Es posible que no puedas iniciar sesión sin habilitarlos primero.","rate_limit":"Por favor, espera un poco antes intentar iniciar sesión de nuevo.","blank_username":"Por favor, ingresa tu correo electrónico o nombre de usuario.","blank_username_or_password":"Por favor, ingresa tu correo electrónico o nombre de usuario y tu contraseña.","reset_password":"Restablecer contraseña","logging_in":"Iniciando Sesión...","or":"O","authenticating":"Autenticando...","awaiting_activation":"Tu cuenta está pendiente de activación, usa el enlace de «olvidé contraseña» para recibir otro correo electrónico de activación.","awaiting_approval":"Tu cuenta todavía no ha sido aprobada por un miembro del staff. Recibirás un correo electrónico cuando sea aprobada.","requires_invite":"Lo sentimos, solo se puede acceder a este foro mediante invitación.","not_activated":"No puedes iniciar sesión todavía. Anteriormente te hemos enviado un correo electrónico de activación a la dirección \u003cb\u003e%{sentTo}\u003c/b\u003e. Por favor, sigue las instrucciones que allí se encuentran para activar tu cuenta.","not_allowed_from_ip_address":"No puedes iniciar sesión desde esa dirección IP.","admin_not_allowed_from_ip_address":"No puedes iniciar sesión como administrador desde esta dirección IP.","resend_activation_email":"Has clic aquí para enviar el correo electrónico de activación nuevamente.","omniauth_disallow_totp":"Tu cuenta tiene la autenticación de dos factores activada. Por favor, inicia sesión con tu contraseña.","resend_title":"Volver a enviar el correo electrónico de activación","change_email":"Cambiar dirección de correo electrónico","provide_new_email":"Ingresa una dirección de correo electrónico nueva y te reenviaremos el correo electrónico de confirmación.","submit_new_email":"Actualizar dirección de correo electrónico","sent_activation_email_again":"Te hemos enviado otro correo electrónico de activación a \u003cb\u003e%{currentEmail}\u003c/b\u003e. Podría tardar algunos minutos en llegar; asegúrate de revisar la carpeta de correo no deseado.","sent_activation_email_again_generic":"Te hemos enviado otro correo  electrónico de activación. Podría tardar algunos minutos en llegar. Asegúrate de revisar la carpeta de correo no deseado.","to_continue":"Por favor, inicia sesión","preferences":"Debes iniciar sesión para poder cambiar tus preferencias de usuario.","not_approved":"Tu cuenta aún no ha sido aprobada. Se te notificará por correo electrónico cuando todo esté listo para que inicies sesión.","google_oauth2":{"name":"Google","title":"con Google"},"twitter":{"name":"Twitter","title":"con Twitter"},"instagram":{"name":"Instagram","title":"con Instagram"},"facebook":{"name":"Facebook","title":"con Facebook"},"github":{"name":"GitHub","title":"con GitHub"},"discord":{"name":"Discord","title":"con Discord"},"second_factor_toggle":{"totp":"Usar una aplicación de autenticación en su lugar","backup_code":"Usar un código de respaldo en su lugar"}},"invites":{"accept_title":"Invitación","emoji":"emoji de un sobre","welcome_to":"¡Bienvenido a %{site_name}!","invited_by":"Has sido invitado por:","social_login_available":"También tendrás la posibilidad de iniciar sesión mediante cualquier red social asociada a ese correo electrónico.","your_email":"El correo electrónico de tu cuenta es \u003cb\u003e%{email}\u003c/b\u003e.","accept_invite":"Aceptar invitación","success":"Tu cuenta ha sido creada y has iniciado sesión.","name_label":"Nombre","password_label":"Contraseña","optional_description":"(opcional)"},"password_reset":{"continue":"Continuar a %{site_name}"},"emoji_set":{"apple_international":"Apple/Internacional","google":"Google","twitter":"Twitter","emoji_one":"JoyPixels (anteriormente EmojiOne)","win10":"Win10","google_classic":"Google Classic","facebook_messenger":"Facebook Messenger"},"category_page_style":{"categories_only":"Solo categorías","categories_with_featured_topics":"Categorías con temas destacados","categories_and_latest_topics":"Categorías y temas recientes","categories_and_top_topics":"Categorías y temas más importantes","categories_boxes":"Cajas con subcategorías","categories_boxes_with_topics":"Cajas con temas destacados"},"shortcut_modifier_key":{"shift":"Shift","ctrl":"Ctrl","alt":"Alt","enter":"Intro"},"conditional_loading_section":{"loading":"Cargando..."},"category_row":{"topic_count":{"one":"%{count} tema en esta categoría","other":"%{count} temas en esta categoría"},"plus_subcategories_title":{"one":"%{name} y una subcategoría","other":"%{name} y %{count} subcategorías"},"plus_subcategories":{"one":"%{count} subcategoría +","other":"%{count} subcategorías +"}},"select_kit":{"filter_by":"Filtrar por: %{name}","select_to_filter":"Selecciona un valor para filtrar","default_header_text":"Seleccionar...","no_content":"No se encontraron coincidencias","filter_placeholder":"Buscar...","filter_placeholder_with_any":"Buscar o crear...","create":"Crear: «%{content}»","max_content_reached":{"one":"Puedes seleccionar únicamente %{count} item.","other":"Solo puedes seleccionar %{count} elementos."},"min_content_not_reached":{"one":"Seleccionar al menos %{count} item.","other":"Selecciona al menos %{count} elementos."},"invalid_selection_length":{"one":"Selecciona al menos %{count} carácter.","other":"Selecciona al menos %{count} caracteres."},"components":{"categories_admin_dropdown":{"title":"Gestionar categorías"}}},"date_time_picker":{"from":"Desde","to":"Hasta"},"emoji_picker":{"filter_placeholder":"Buscar emoji","smileys_\u0026_emotion":"Caras y emociones","people_\u0026_body":"Personas y cuerpo","animals_\u0026_nature":"Animales y naturaleza","food_\u0026_drink":"Comida y bebida","travel_\u0026_places":"Viajes y lugares","activities":"Actividades","objects":"Objetos","symbols":"Símbolos","flags":"Banderas","recent":"Usados recientemente","default_tone":"Sin tono de piel","light_tone":"Tono de piel claro","medium_light_tone":"Tono de piel medio claro","medium_tone":"Tono de piel medio","medium_dark_tone":"Tono de piel medio oscuro","dark_tone":"Tono de piel oscuro","default":"Emojis personalizados"},"shared_drafts":{"title":"Borradores Compartidos","notice":"Este tema solo es visible para aquellos que pueden publicar borradores compartidos.","destination_category":"Categoría de destino","publish":"Publicar borrador compartido","confirm_publish":"¿Estás seguro de que quieres publicar este borrador?","publishing":"Publicando Tema..."},"composer":{"emoji":"Emoji :)","more_emoji":"más...","options":"Opciones","whisper":"susurrar","unlist":"invisible","add_warning":"Esta es una advertencia oficial.","toggle_whisper":"Activar/desactivar susurro","toggle_unlisted":"Visible/Invisible","posting_not_on_topic":"¿A qué tema quieres responder?","saved_local_draft_tip":"guardado localmente","similar_topics":"Tu tema es similar a...","drafts_offline":"borradores sin conexión","edit_conflict":"conflicto de edición","group_mentioned_limit":{"one":"\u003cb\u003eAviso\u003c/b\u003e Has mencionado al grupo \u003ca href='%{group_link}'\u003e%{group}\u003c/a\u003e, pero tiene más miembros que los que los administradores permiten mencionar (%{count} usuario) de una vez. No se notificará a nadie.","other":"\u003cb\u003eAviso\u003c/b\u003e Has mencionado al grupo \u003ca href='%{group_link}'\u003e%{group}\u003c/a\u003e, pero tiene más miembros que los que los administradores permiten mencionar (%{count} usuarios) de una vez. No se notificará a nadie."},"group_mentioned":{"one":"Al mencionar a %{group}, estás a punto de notificar a \u003ca href='%{group_link}'\u003e%{count} persona\u003c/a\u003e – ¿seguro que quieres hacerlo?","other":"Al mencionar a %{group}, estás a punto de notificar a \u003ca href='%{group_link}'\u003e%{count} personas\u003c/a\u003e ¿Estás seguro de que quieres hacerlo?"},"cannot_see_mention":{"category":"Mencionaste a %{username} pero no se les notificará porque no tienen acceso a esta categoría. Necesitarás añadirlos a un grupo que tenga acceso a esta categoría.","private":"Mencionaste a %{username} pero no se les notificará porque no pueden ver este mensaje personal. Necesitarás invitarlos a este MP."},"duplicate_link":"Parece que tu enlace a \u003cb\u003e%{domain}\u003c/b\u003e ya se publicó en el tema por \u003cb\u003e@%{username}\u003c/b\u003e en \u003ca href='%{post_url}'\u003euna respuesta el %{ago}\u003c/a\u003e. ¿Estás seguro de que deseas volver a publicarlo?","reference_topic_title":"RE: %{title}","error":{"title_missing":"Es necesario un título","post_missing":"La publicación no puede estar vacía","post_length":{"one":"La publicación debe tener por lo menos %{count} caracteres","other":"La publicación debe tener por lo menos %{count} caracteres"},"try_like":"¿Has probado el botón %{heart}?","category_missing":"Debes escoger una categoría.","tags_missing":{"one":"Debes seleccionar al menos %{count} etiqueta","other":"Debes seleccionar al menos %{count} etiquetas"},"topic_template_not_modified":"Por favor, agrega detalles y especificaciones a tu tema editando la plantilla de tema."},"save_edit":"Guardar edición","overwrite_edit":"Sobrescribir edición","reply_original":"Responder en el tema original","reply_here":"Responder aquí","reply":"Responder","cancel":"Cancelar","create_topic":"Crear tema","create_pm":"Mensaje","create_whisper":"Susurrar","create_shared_draft":"Crear borrador compartido","edit_shared_draft":"Editar borrador compartido","title":"O pulsa Ctrl+Intro","users_placeholder":"Añadir un usuario","title_placeholder":"En una frase breve, ¿de qué trata este tema?","title_or_link_placeholder":"Escribe un título o pega un enlace aquí","edit_reason_placeholder":"¿Por qué lo estás editando?","topic_featured_link_placeholder":"Ingresa el enlace mostrado con el título.","remove_featured_link":"Eliminar enlace del tema.","reply_placeholder":"Escribe aquí. Usa Markdown, BBCode o HTML para darle formato. Arrastra o pega imágenes.","reply_placeholder_no_images":"Escribe aquí. Usa Markdown, BBCode o HTML para darle formato.","reply_placeholder_choose_category":"Selecciona una categoría antes de escribir aquí.","view_new_post":"Ver tu publicación nueva.","saving":"Guardando","saved":"¡Guardado!","saved_draft":"Publica el borrador en progreso. Escribe para reanudar.","uploading":"Subiendo...","show_preview":"mostrar vista previa \u0026raquo;","hide_preview":"\u0026laquo; ocultar vista previa","quote_post_title":"Citar toda la publicación","bold_label":"B","bold_title":"Negrita","bold_text":"texto en negrita","italic_label":"I","italic_title":"Cursiva","italic_text":"Texto en cursiva","link_title":"Hipervínculo","link_description":"Ingresa la descripción del enlace aquí","link_dialog_title":"Insertar hipervínculo","link_optional_text":"título opcional","link_url_placeholder":"Copia una URL o escribe para buscar temas","blockquote_title":"Cita","blockquote_text":"Cita","code_title":"Texto preformateado","code_text":"texto preformateado con sangría de 4 espacios","paste_code_text":"escribe o pega el código aquí","upload_title":"Subir","upload_description":"Ingresa una descripción del archivo subido aquí","olist_title":"Lista numerada","ulist_title":"Lista con viñetas","list_item":"Lista de elementos","toggle_direction":"Alternar dirección","help":"Ayuda de edición con Markdown","collapse":"minimizar el panel de edición","open":"abrir el panel de composición","abandon":"cerrar el editor y descartar borrador","enter_fullscreen":"ingresar al editor en pantalla completa","exit_fullscreen":"salir del editor en pantalla completa","show_toolbar":"mostrar la barra de herramientas del editor","hide_toolbar":"ocultar la barra de herramientas del editor","modal_ok":"OK","modal_cancel":"Cancelar","cant_send_pm":"Lo sentimos, no puedes enviar un mensaje a %{username}.","yourself_confirm":{"title":"¿Olvidaste añadir destinatarios?","body":"¡Vas a enviarte este mensaje solo a ti mismo!"},"slow_mode":{"error":"Este tema está en modo lento. Para promover un debate considerado y adecuado solo puedes publicar una vez cada %{duration}."},"admin_options_title":"Configuración opcional del administrador para este tema","composer_actions":{"reply":"Responder","draft":"Borrador","edit":"Editar","reply_to_post":{"label":"Responder a una publicación de %{postUsername}","desc":"Responder a una publicación específica"},"reply_as_new_topic":{"label":"Responder como tema enlazado","desc":"Crear un nuevo tema enlazado a este tema","confirm":"Tienes un borrador de tema nuevo guardado. Este se sobrescribirá si creas un tema enlazado."},"reply_as_new_group_message":{"label":"Responder como un nuevo mensaje de grupo","desc":"Crear un nuevo mensaje privado con los mismos destinatarios"},"reply_as_private_message":{"label":"Nuevo mensaje","desc":"Crear un nuevo mensaje personal"},"reply_to_topic":{"label":"Responder al tema","desc":"Responder al tema, no a una publicación en específico"},"toggle_whisper":{"label":"Mostrar/Ocultar Susurros","desc":"Los susurros son visibles solo para los miembros del staff"},"create_topic":{"label":"Crear tema"},"shared_draft":{"label":"Borrador compartido","desc":"Inicia un tema-borrador que solo será visible por los usuarios permitidos"},"toggle_topic_bump":{"label":"Alternar bump del tema","desc":"Responder sin alterar la fecha de última respuesta"}},"reload":"Recargar","ignore":"Ignorar","details_title":"Resumen","details_text":"Este texto estará oculto"},"notifications":{"tooltip":{"regular":{"one":"%{count} notificación sin leer","other":"%{count} notificaciones no leídas"},"message":{"one":"%{count} mensaje sin leer","other":"%{count} mensajes sin leer"},"high_priority":{"one":"%{count} notificación de alta prioridad sin leer","other":"%{count} notificaciones de alta prioridad sin leer"}},"title":"notificaciones por menciones a tu @nombre, respuestas a tus publicaciones y temas, mensajes, etc","none":"No se pudieron cargar las notificaciones en este momento.","empty":"No se encontraron notificaciones.","post_approved":"Tu publicación ha sido aprobada","reviewable_items":"elementos que requieren revisión","mentioned":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","group_mentioned":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","quoted":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","bookmark_reminder":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","replied":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","posted":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","edited":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","liked":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","liked_2":"\u003cspan class='double-user'\u003e%{username}, %{username2}\u003c/span\u003e %{description}","liked_many":{"one":"\u003cspan class='multi-user'\u003e%{username}, %{username2} y %{count} más\u003c/span\u003e %{description}","other":"\u003cspan class='multi-user'\u003e%{username}, %{username2} y %{count} más\u003c/span\u003e %{description}"},"liked_consolidated_description":{"one":"le ha dado me gusta a %{count} de tus mensajes","other":"le ha dado me gusta a %{count} de tus publicaciones"},"liked_consolidated":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","private_message":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","invited_to_private_message":"\u003cp\u003e\u003cspan\u003e%{username}\u003c/span\u003e %{description}","invited_to_topic":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","invitee_accepted":"\u003cspan\u003e%{username}\u003c/span\u003e aceptó tu invitación","moved_post":"\u003cspan\u003e%{username}\u003c/span\u003e movió %{description}","linked":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","granted_badge":"Ganaste '%{description}'","topic_reminder":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","watching_first_post":"\u003cspan\u003eNuevo tema\u003c/span\u003e %{description}","membership_request_accepted":"Membresía aceptada en «%{group_name}»","membership_request_consolidated":{"one":"%{count} solicitude de membresía pendiente para el grupo «%{group_name}»","other":"%{count} solicitudes de membresía pendientes para el grupo «%{group_name}»"},"reaction":"\u003cspan\u003e%{username}\u003c/span\u003e %{description}","reaction_2":"\u003cspan\u003e%{username} y %{username2}\u003c/span\u003e %{description}","votes_released":"%{description} - completado","group_message_summary":{"one":"%{count} mensaje en tu bandeja de %{group_name}","other":"%{count} mensajes en tu bandeja de %{group_name} "},"popup":{"mentioned":"%{username} te mencionó en «%{topic}» - %{site_title}","group_mentioned":"%{username} te mencionó en «%{topic}» - %{site_title}","quoted":"%{username} te citó en «%{topic}» - %{site_title}","replied":"%{username} te respondió en «%{topic}» - %{site_title}","posted":"%{username} publicó en «%{topic}» - %{site_title}","private_message":"%{username} te envió un mensaje personal en «%{topic}» - %{site_title}","linked":"%{username} enlazó tu publicación desde «%{topic}» - %{site_title}","watching_first_post":"%{username} creó un nuevo tema «%{topic}» - %{site_title}","confirm_title":"Notificaciones activadas - %{site_title}","confirm_body":"¡Éxito! Se han activado las notificaciones.","custom":"Notificación de %{username} en %{site_title}"},"titles":{"mentioned":"mencionado","replied":"nueva respuesta","quoted":"citado","edited":"editado","liked":"nuevo me gusta","private_message":"nuevo mensaje privado","invited_to_private_message":"invitado a mensaje privado","invitee_accepted":"invitación aceptada","posted":"nueva publicación","moved_post":"publicación movida","linked":"enlazado","bookmark_reminder":"recordatorio de marcador","bookmark_reminder_with_name":"recordatorio de marcador - %{name}","granted_badge":"medalla concedida","invited_to_topic":"invitado al tema","group_mentioned":"grupo mencionado","group_message_summary":"nuevos mensajes grupales","watching_first_post":"nuevo tema","topic_reminder":"recordatorio de tema","liked_consolidated":"nuevos me gusta","post_approved":"publicación aprobada","membership_request_consolidated":"nuevas solicitudes de membresía","reaction":"nueva reacción","votes_released":"Voto liberado"},"alt":{"solved":{"accepted_notification":"aceptado"}}},"upload_selector":{"title":"Agregar imagen","title_with_attachments":"Agregar una imagen o archivo","from_my_computer":"Desde mi dispositivo","from_the_web":"Desde la web","remote_tip":"enlace a la imagen","remote_tip_with_attachments":"enlace a imagen o archivo %{authorized_extensions}","local_tip":"selecciona las imágenes de tu dispositivo","local_tip_with_attachments":"selecciona imágenes o archivos de tu dispositivo %{authorized_extensions}","hint":"(también puedes arrastrarlos al editor para subirlos)","hint_for_supported_browsers":"puedes también arrastrar o pegar imágenes en el editor","uploading":"Subiendo","select_file":"Selecciona archivo","default_image_alt_text":"imagen"},"search":{"sort_by":"Ordenar por","relevance":"Relevancia","latest_post":"Publicación más reciente","latest_topic":"Tema más reciente","most_viewed":"Más visto","most_liked":"Más me gusta recibidos","select_all":"Seleccionar todo","clear_all":"Limpiar todo","too_short":"El término de búsqueda es demasiado corto.","result_count":{"one":"\u003cspan\u003e%{count} resultado para\u003c/span\u003e\u003cspan class='term'\u003e%{term}\u003c/span\u003e","other":"\u003cspan\u003e%{count}%{plus} resultados para\u003c/span\u003e\u003cspan class='term'\u003e%{term}\u003c/span\u003e"},"title":"buscar temas, publicaciones, usuarios o categorías","full_page_title":"buscar temas o publicaciones","no_results":"No se encontró ningún resultado.","no_more_results":"No se encontraron más resultados.","post_format":"#%{post_number} de %{username}","results_page":"Resultados de búsqueda de «%{term}»","more_results":"Hay más resultados. Por favor, restringe los criterios de búsqueda.","cant_find":"¿No puedes encontrar lo que estás buscando?","start_new_topic":"¿Y si creas un nuevo tema?","or_search_google":"O prueba buscar a través de Google:","search_google":"Intenta buscar con Google:","search_google_button":"Google","search_button":"Buscar","context":{"user":"Buscar publicaciones de @%{username}","category":"Buscar la categoría #%{category}","tag":"Buscar la etiqueta #%{tag} ","topic":"Buscar en este tema","private_messages":"Buscar en mensajes"},"advanced":{"title":"Búsqueda avanzada","posted_by":{"label":"Publicado por"},"in_category":{"label":"Categoría"},"in_group":{"label":"En el grupo"},"with_badge":{"label":"Con la medalla"},"with_tags":{"label":"Etiquetas"},"filters":{"label":"Solo temas/mensajes que...","title":"coincide el título únicamente","likes":"me han gustado","posted":"he publicado en ellos","created":"Creado por mi","watching":"estoy vigilando","tracking":"estoy siguiendo","private":"en mis mensajes","bookmarks":"he guardado","first":"son la primera publicación","pinned":"son destacados","seen":"he leído","unseen":"no he leído","wiki":"son tipo wiki","images":"incluyen imágenes","all_tags":"todas las etiquetas anteriores"},"statuses":{"label":"Donde los temas","open":"están abiertos","closed":"están cerrados","public":"son públicos","archived":"están archivados","noreplies":"no tienen respuestas","single_user":"contienen un solo usuario","solved":"están resueltos","unsolved":"están sin resolver"},"post":{"count":{"label":"Publicaciones"},"min":{"placeholder":"mínimo"},"max":{"placeholder":"máximo"},"time":{"label":"Fecha de publicación","before":"antes de","after":"después de"}},"views":{"label":"Visitas"},"min_views":{"placeholder":"mínimo"},"max_views":{"placeholder":"máximo"}}},"hamburger_menu":"ir a otra lista de temas o categoría","new_item":"nuevo","go_back":"volver","not_logged_in_user":"página de usuario con resumen de la actividad y preferencias actuales","current_user":"ir a tu página de usuario","view_all":"ver todo","topics":{"new_messages_marker":"última visita","bulk":{"select_all":"Seleccionar todos","clear_all":"Desmarcar todos","unlist_topics":"Hacer invisibles","relist_topics":"Hacer visibles de nuevo","reset_read":"Restablecer leídos","delete":"Eliminar temas","dismiss":"Descartar","dismiss_read":"Descartar todos los temas sin leer","dismiss_button":"Descartar...","dismiss_tooltip":"Descartar solamente las nuevas publicaciones o dejar de seguir los temas","also_dismiss_topics":"Dejar de seguir estos temas para que no aparezcan más en mis mensajes no leídos","dismiss_new":"Ignorar nuevos","toggle":"activar selección de temas en bloque","actions":"Acciones en bloque","change_category":"Cambiar categoría","close_topics":"Cerrar temas","archive_topics":"Archivar temas","notification_level":"Notificaciones","change_notification_level":"Cambiar nivel de notificación","choose_new_category":"Elige la nueva categoría de los temas:","selected":{"one":"Has seleccionado \u003cb\u003e%{count}\u003c/b\u003e tema.","other":"Has seleccionado \u003cb\u003e%{count}\u003c/b\u003e temas."},"change_tags":"Remplazar etiquetas","append_tags":"Agregar etiquetas","choose_new_tags":"Elige etiquetas nuevas para estos temas:","choose_append_tags":"Elegir etiquetas nuevas para agregar a estos temas:","changed_tags":"Las etiquetas de esos temas fueron cambiadas.","remove_tags":"Eliminar todas las etiquetas","confirm_remove_tags":{"one":"Todas las etiquetas se eliminarán de este tema. ¿Estás seguro?","other":"Todas las etiquetas se eliminarán de \u003cb\u003e%{count}\u003c/b\u003e temas. ¿Estás seguro?"},"progress":{"one":"Progreso: \u003cstrong\u003e%{count}\u003c/strong\u003e tema","other":"Progreso: \u003cstrong\u003e%{count}\u003c/strong\u003e temas"}},"none":{"unread":"No tienes temas no leídos.","new":"No tienes temas nuevos.","read":"Todavía no has leído ningún tema.","posted":"Todavía no has publicado en ningún tema.","ready_to_create":"Listo para ","latest":"¡Estás al día!","bookmarks":"Todavía no tienes temas guardados en marcadores.","category":"No hay temas con la categoría %{category}.","top":"No hay temas destacados.","educate":{"new":"\u003cp\u003eTus nuevos temas aparecerán aquí. Por defecto, los temas se consideran nuevos y mostrarán un \u003cspan class=\"badge new-topic badge-notification\" style=\"vertical-align:middle;line-height:inherit;\"\u003e\u003c/span\u003e indicador si se crearon hace menos de 2 días.\u003c/p\u003e\u003cp\u003eVisita tus \u003ca href=\"%{userPrefsUrl}\"\u003epreferencias\u003c/a\u003e para cambiarlo.\u003c/p\u003e","unread":"\u003cp\u003eTus temas sin leer aparecen aquí.\u003c/p\u003e\u003cp\u003ePor defecto, los temas se consideran sin leer y mostrarán contadores de publicaciones sin leer \u003cspan class=\"badge new-posts badge-notification\"\u003e1\u003c/span\u003e si tu:\u003c/p\u003e\u003cul\u003e\u003cli\u003eCreaste el tema\u003c/li\u003e\u003cli\u003eRespondiste al tema\u003c/li\u003e\u003cli\u003eLeíste el tema por más de 4 minutos\u003c/li\u003e\u003c/ul\u003e\u003cp\u003eO si has establecido específicamente el tema como seguido o vigilado a través del control de notificaciones al pie de cada tema.\u003c/p\u003e\u003cp\u003eConfigura tus \u003ca href=\"%{userPrefsUrl}\"\u003epreferencias\u003c/a\u003e para cambiar esto.\u003c/p\u003e"}},"bottom":{"latest":"No hay más temas recientes.","posted":"No hay más temas publicados.","read":"No hay más temas leídos.","new":"No hay más temas nuevos.","unread":"No hay más temas que no hayas leído.","category":"No hay más temas de la categoría %{category}.","tag":"No hay más temas con la etiqueta %{tag}.","top":"No hay más temas destacados.","bookmarks":"No hay más temas guardados en marcadores."}},"topic":{"filter_to":{"one":"%{count} publicación en el tema","other":"%{count} publicaciones en el tema"},"create":"Crear tema","create_long":"Crear un tema nuevo","open_draft":"Abrir borrador","private_message":"Comenzar un mensaje","archive_message":{"help":"Archivar mensaje","title":"Archivar"},"move_to_inbox":{"title":"Mover a la bandeja de entrada","help":"Restaurar mensaje a la bandeja de entrada"},"edit_message":{"help":"Editar la primera publicación del mensaje","title":"Editar"},"defer":{"help":"Marcar como no leído","title":"Aplazar"},"feature_on_profile":{"help":"Añadir un enlace a este tema en tu tarjeta de usuario y perfil","title":"Destacar en el perfil"},"remove_from_profile":{"warning":"Tu perfil ya tiene un tema destacado. Si continúas, este tema remplazará el tema actual.","help":"Eliminar el enlace a este tema de tu perfil de usuario","title":"Eliminar del perfil"},"list":"Temas","new":"nuevo tema","unread":"sin leer","new_topics":{"one":"%{count} tema nuevo","other":"%{count} temas nuevos"},"unread_topics":{"one":"%{count} tema sin leer","other":"%{count} temas sin leer"},"title":"Tema","invalid_access":{"title":"Este tema es privado","description":"Lo sentimos, ¡no tienes acceso a este tema!","login_required":"Debes iniciar sesión para poder ver este tema."},"server_error":{"title":"No se pudo cargar el tema","description":"Lo sentimos, no pudimos cargar el tema. Posiblemente se debe a problemas de conexión. Por favor, inténtalo nuevamente más tarde. Si el problema persiste, por favor contáctanos."},"not_found":{"title":"Tema no encontrado","description":"Lo sentimos, no pudimos encontrar ese tema. ¿Tal vez fue eliminado por un moderador?"},"total_unread_posts":{"one":"tienes %{count} publicación sin leer en este tema","other":"tienes %{count} publicaciones sin leer en este tema"},"unread_posts":{"one":"tienes %{count} publicación antigua sin leer en este tema","other":"tienes %{count} publicaciones antiguas sin leer en este tema"},"new_posts":{"one":"hay %{count} nueva publicación en este tema desde la última vez que lo leíste","other":"hay %{count} publicaciones nuevas en este tema desde la última vez que lo leíste"},"likes":{"one":"este tema le gusta a %{count} persona","other":"este tema les gusta a %{count} personas"},"back_to_list":"Volver a la lista de temas","options":"Opciones del tema","show_links":"mostrar enlaces dentro de este tema","toggle_information":"activar/desactivar detalles del tema","read_more_in_category":"¿Quieres leer más? Consulta otros temas en %{catLink} o %{latestLink}.","read_more":"¿Quieres leer más? %{catLink} o %{latestLink}.","unread_indicator":"Ningún miembro ha leído todavía la última publicación de este tema.","browse_all_categories":"Ver todas las categorías","browse_all_tags":"Ver todas las etiquetas","view_latest_topics":"ver los temas recientes","suggest_create_topic":"¿iniciar una nueva conversación?","jump_reply_up":"saltar a la primera respuesta","jump_reply_down":"saltar a la última respuesta","deleted":"El tema ha sido eliminado","slow_mode_update":{"title":"Modo lento","select":"Los usuarios solo pueden publicar en este tema una vez cada:","description":"Para promover un debate considerado en temas con mucho tráfico o controvertidos, los usuarios deberán esperar antes de volver a publicar en este tema.","save":"Activar","enabled_until":"(Opcional) Activado hasta:","remove":"Desactivar","hours":"Horas:","minutes":"Minutos:","seconds":"Segundos:","durations":{"15_minutes":"15 minutos","1_hour":"1 hora","4_hours":"4 horas","1_day":"1 día","1_week":"1 semana","custom":"Otra duración"}},"slow_mode_notice":{"duration":"Debes esperar %{duration} entre respuestas a este tema."},"topic_status_update":{"title":"Temporizador de temas","save":"Configurar temporizador","num_of_hours":"Número de horas:","num_of_days":"Número de días:","remove":"Quitar temporizador","publish_to":"Publicar en:","when":"Cuando:","time_frame_required":"Por favor, selecciona un plazo"},"auto_update_input":{"none":"Selecciona el plazo","now":"Ahora","later_today":"Más tarde durante el día de hoy","tomorrow":"Mañana","later_this_week":"Esta misma semana","this_weekend":"Este fin de semana","next_week":"Próxima semana","two_weeks":"Dos semanas","next_month":"Próximo mes","two_months":"Dos meses","three_months":"Tres meses","four_months":"Cuatro meses","six_months":"Seis meses","one_year":"Un año","forever":"Para siempre","pick_date_and_time":"Selecciona fecha y horario","set_based_on_last_post":"Cerrar en función de la última publicación"},"publish_to_category":{"title":"Programar publicación"},"temp_open":{"title":"Abrir temporalmente"},"auto_reopen":{"title":"Abrir tema automaticamente"},"temp_close":{"title":"Cerrar temporalmente"},"auto_close":{"title":"Cerrar tema automaticamente","label":"Horas de cierre automático del tema:","error":"Por favor, ingresa un valor válido.","based_on_last_post":"No cerrar hasta que la última publicación en el tema tenga por lo menos esta antigüedad."},"auto_delete":{"title":"Eliminar tema automaticamente"},"auto_bump":{"title":"Hacer bump al tema automaticamente"},"reminder":{"title":"Recordarme"},"auto_delete_replies":{"title":"Eliminar respuestas automáticamente"},"status_update_notice":{"auto_open":"Este tema se abrirá automáticamente %{timeLeft}.","auto_close":"Este tema se cerrará automáticamente %{timeLeft}.","auto_publish_to_category":"Este tema se publicará en \u003ca href=%{categoryUrl}\u003e#%{categoryName}\u003c/a\u003e %{timeLeft}.","auto_close_based_on_last_post":"Este tema se cerrará %{duration} después de la última respuesta.","auto_delete":"Este tema se eliminará automáticamente %{timeLeft}.","auto_bump":"La fecha de este tema se actualizará %{timeLeft}.","auto_reminder":"Te recordaremos sobre este tema %{timeLeft}.","auto_delete_replies":"Las respuestas a este tema serán eliminadas automáticamente después de %{duration}."},"auto_close_title":"Configuración de cierre automático","auto_close_immediate":{"one":"La última publicación se realizó hace %{count} hora, por lo que el tema se cerrará inmediatamente.","other":"La última publicación se realizó hace %{count} horas, por lo que el tema se cerrará inmediatamente."},"timeline":{"back":"Volver","back_description":"Volver a la última publicación sin leer","replies_short":"%{current} / %{total}"},"progress":{"title":"avances","go_top":"arriba","go_bottom":"abajo","go":"ir","jump_bottom":"saltar a la última publicación","jump_prompt":"saltar a...","jump_prompt_long":"Saltar a...","jump_bottom_with_number":"saltar a la publicación %{post_number}","jump_prompt_to_date":"hasta hoy","jump_prompt_or":"o","total":"total de publicaciones","current":"publicación actual"},"notifications":{"title":"cambiar la frecuencia con la que se te notifica acerca de este tema","reasons":{"mailing_list_mode":"El modo lista de correo se encuentra activado, por lo que se te notificarán las respuestas a este tema por correo electrónico.","3_10":"Recibirás notificaciones porque estás vigilando una etiqueta de este tema.","3_6":"Recibirás notificaciones porque estás vigilando esta categoría.","3_5":"Recibirás notificaciones porque has empezado a vigilar este tema automáticamente.","3_2":"Recibirás notificaciones porque estás vigilando este tema.","3_1":"Recibirás notificaciones porque creaste este tema.","3":"Recibirás notificaciones porque estás vigilando este tema.","2_8":"Verás el número de respuestas  nuevas porque estás siguiendo esta categoría.","2_4":"Verás el número de respuestas nuevas porque has publicado una respuesta en este tema.","2_2":"Verás el número de respuestas nuevas porque estás siguiendo este tema.","2":"Verás un contador con el número de nuevas respuestas porque has \u003ca href=\"%{basePath}/u/%{username}/preferences/notifications\"\u003eleído este tema\u003c/a\u003e.","1_2":"Se te notificará si alguien menciona tu @nombre o te responde.","1":"Se te notificará si alguien menciona tu @nombre o te responde.","0_7":"Estás ignorando todas las notificaciones en esta categoría.","0_2":"Estás ignorando todas las notificaciones en este tema.","0":"Estás ignorando todas las notificaciones en este tema."},"watching_pm":{"title":"Vigilar","description":"Se te notificará de cada publicación nueva en este mensaje y se mostrará el número de publicaciones nuevas."},"watching":{"title":"Vigilar","description":"Se te notificará de cada publicación nueva en este tema y se mostrará el número de publicaciones nuevas."},"tracking_pm":{"title":"Seguir","description":"Se mostrará el número de respuestas nuevas a este mensaje y se te notificará si alguien menciona tu @nombre o te responde."},"tracking":{"title":"Seguir","description":"Se mostrará el número de respuestas nuevas en este tema. Se te notificará si alguien menciona tu @nombre o te responde."},"regular":{"title":"Normal","description":"Se te notificará solo si alguien menciona tu @nombre o te responde."},"regular_pm":{"title":"Normal","description":"Se te notificará solo si alguien menciona tu @nombre o te responde."},"muted_pm":{"title":"Silenciar","description":"No recibirás ninguna notificación de este mensaje."},"muted":{"title":"Silenciar","description":"No recibirás ninguna notificación de este tema y no aparecerá en temas recientes."}},"actions":{"title":"Acciones","recover":"Deshacer eliminar tema","delete":"Eliminar tema","open":"Abrir tema","close":"Cerrar tema","multi_select":"Seleccionar publicaciones...","slow_mode":"Ajustar modo lento","timed_update":"Configurar temporizador de temas...","pin":"Destacar tema...","unpin":"Dejar de destacar...","unarchive":"Desarchivar tema","archive":"Archivar tema","invisible":"Hacer invisible","visible":"Hacer visible","reset_read":"Restablecer datos de lectura","make_public":"Convertir en tema público","make_private":"Crear mensaje personal","reset_bump_date":"Resetear fecha de bump"},"feature":{"pin":"Destacar tema","unpin":"Dejar de destacar tema","pin_globally":"Destacar tema globalmente","make_banner":"Tema de encabezado","remove_banner":"Quitar tema de encabezado"},"reply":{"title":"Responder","help":"comienza a escribir un mensaje en este tema"},"clear_pin":{"title":"Eliminar destacado","help":"Dejar de descatar este tema para que no aparezca más de primero en tu lista de temas"},"share":{"title":"Compartir","extended_title":"Compartir un enlace","help":"comparte el enlace a este tema"},"print":{"title":"Imprimir","help":"Abrir una versión imprimible de este tema"},"flag_topic":{"title":"Reportar","help":"reportar de forma privada que se requiere atención o enviar una notificación privada","success_message":"Has reportado este tema correctamente."},"make_public":{"title":"Convertir en tema público","choose_category":"Por favor, elige una categoría para el tema público:"},"feature_topic":{"title":"Características de este tema","pin":"Hacer que este tema aparezca de primero en la categoría %{categoryLink} hasta","unpin":"Quitar este tema del principio de la lista en la categoría %{categoryLink}.","unpin_until":"Quitar este tema del top de la categoría %{categoryLink} o esperar al \u003cstrong\u003e%{until}\u003c/strong\u003e.","pin_note":"Los usuarios pueden dejar de destacar los temas de forma individual por sí mismos.","pin_validation":"Es obligatorio especificar una fecha para destacar este tema.","not_pinned":"No hay temas destacados en %{categoryLink}.","already_pinned":{"one":"Tema destacado actualmente en %{categoryLink}: \u003cstrong class='badge badge-notification unread'\u003e%{count}\u003c/strong\u003e","other":"Temas destacados actualmente en %{categoryLink}: \u003cstrong class='badge badge-notification unread'\u003e%{count}\u003c/strong\u003e"},"pin_globally":"Hacer que este tema aparezca de primero en todas las listas de temas hasta","confirm_pin_globally":{"one":"Ya tienes %{count} tema anclado globalmente. Demasiados temas anclados pueden suponer una carga para usuarios nuevos y anónimos. ¿Seguro que quieres anclar otro tema globalmente?","other":"Ya tienes %{count} temas anclados globalmente. Demasiados temas anclados pueden suponer una carga para usuarios nuevos y anónimos. ¿Seguro que quieres anclar otro tema globalmente?"},"unpin_globally":"Quitar este tema de la parte superior de todas las listas de temas.","unpin_globally_until":"Quitar este tema de la parte superior de todas las listas de temas o esperar hasta \u003cstrong\u003e%{until}\u003c/strong\u003e.","global_pin_note":"Los usuarios pueden dejar de destacar el tema de forma individual por sí mismos.","not_pinned_globally":"No hay temas destacados globalmente.","already_pinned_globally":{"one":"Actualmente hay \u003cstrong class='badge badge-notification unread'\u003e%{count}\u003c/strong\u003e tema destacado globalmente.","other":"Temas destacados globalmente: \u003cstrong class='badge badge-notification unread'\u003e%{count}\u003c/strong\u003e"},"make_banner":"Hacer que este tema aparezca como encabezado en la parte superior de todas las páginas.","remove_banner":"Retire el encabezado que aparece en la parte superior de todas las páginas.","banner_note":"Los usuarios pueden cerrar el encabezado y descartarlo. Solo se puede anunciar un tema a la vez.","no_banner_exists":"No hay ningún tema como encabezado.","banner_exists":"Actualmente \u003cstrong class='badge badge-notification unread'\u003ehay\u003c/strong\u003e un tema como encabezado."},"inviting":"Invitando...","automatically_add_to_groups":"Esta invitación incluye además acceso a los siguientes grupos:","invite_private":{"title":"Invitar al mensaje","email_or_username":"Correo electrónico o nombre de usuario del invitado","email_or_username_placeholder":"correo electrónico o nombre de usuario","action":"Invitar","success":"Hemos invitado a ese usuario a participar en este mensaje.","success_group":"Hemos invitado a ese grupo a participar en este mensaje.","error":"Lo sentimos, se produjo un error al invitar a ese usuario.","not_allowed":"Lo sentimos, no se puede invitar a ese usuario.","group_name":"nombre del grupo"},"controls":"Controles del tema","invite_reply":{"title":"Invitar","username_placeholder":"nombre de usuario","action":"Enviar invitación","help":"invitar a otros a este tema mediante correo electrónico o notificaciones","discourse_connect_enabled":"Ingresa el nombre de usuario o correo electrónico de la persona que deseas invitar a este tema.","to_topic_blank":"Ingresa el nombre de usuario o correo electrónico de la persona que desea invitar a este tema.","to_topic_email":"Ingresaste una dirección de correo electrónico. Nosotros enviaremos una invitación a tu amigo que le permitirá responder inmediatamente a este tema.","to_topic_username":"Ingresaste un nombre de usuario. Le enviaremos una notificación con un enlace de invitación a este tema.","to_username":"Ingresa el nombre de usuario de la persona a la que quieres invitar. Le enviaremos una notificación con un enlace de invitación a este tema.","email_placeholder":"nombre@ejemplo.com","success_username":" Hemos invitado a ese usuario a participar en este tema.","error":"Lo sentimos, no pudimos invitar a esa persona. ¿Tal vez ya fue invitada antes? (El número de invitaciones es limitado)","success_existing_email":"Ya existe un usuario con el correo \u003cb\u003e%{emailOrUsername}\u003c/b\u003e. Invitamos a ese usuario a participar en este tema."},"login_reply":"Inicia sesión para responder","filters":{"n_posts":{"one":"%{count} publicación","other":"%{count} publicaciones"},"cancel":"Quitar filtro"},"move_to":{"title":"Mover a","action":"mover a","error":"Se produjo un error al mover los mensajes."},"split_topic":{"title":"Mover a un tema nuevo","action":"mover a un tema nuevo","topic_name":"Título del tema nuevo","radio_label":"Tema nuevo","error":"Se produjo un error al mover las publicaciones al nuevo tema","instructions":{"one":"Estas a punto de crear un tema nuevo y rellenarlo con la publicación que has seleccionado.","other":"Estas a punto de crear un tema nuevo y rellenarlo con las \u003cb\u003e%{count}\u003c/b\u003e publiacaciones que has seleccionado."}},"merge_topic":{"title":"Mover a un tema existente","action":"mover a un tema existente","error":"Se produjo un error al mover las publicaciones a ese tema","radio_label":"Tema existente","instructions":{"one":"Por favor escoge el tema al que quieres mover esa publicación.","other":"Por favor, escoge el tema al que quieres mover estas \u003cb\u003e%{count}\u003c/b\u003e publicaciones."}},"move_to_new_message":{"title":"Mover a un mensaje nuevo","action":"mover a un mensaje nuevo","message_title":"Título del mensaje nuevo","radio_label":"Mnesaje nuevo","participants":"Participantes","instructions":{"one":"Estás a punto de crear un nuevo mensaje y de llenarlo con el mensaje que has seleccionado.","other":"Estás a punto de crear un mensaje nuevo y llenarlo con los \u003cb\u003e%{count}\u003c/b\u003e mensajes seleccionados."}},"move_to_existing_message":{"title":"Mover a un mensaje existente","action":"mover a un mensaje existente","radio_label":"Mensaje existente","participants":"Participantes","instructions":{"one":"Por favor, selecciona el mensaje al que te gustaría mover el mensaje.","other":"Por favor, selecciona el mensaje al que te gustaría mover los \u003cb\u003e%{count}\u003c/b\u003e mensajes."}},"merge_posts":{"title":"Fusionar las publicaciones seleccionadas","action":"fusionar las publicaciones seleccionadas","error":"Se produjo un error al fusionar las oublicaciones seleccionadas."},"publish_page":{"title":"Publicación de página","publish":"Publicar","description":"Cuando un tema se publica como página su URL se puede compartir y se mostrará con estilos personalizados.","slug":"Slug","public":"Público","public_description":"Cualquiera puede ver la página aunque el tema asociado sea privado.","publish_url":"La página se ha publicado en:","topic_published":"El tema se ha publicado en:","preview_url":"La página será publicada en:","invalid_slug":"Lo sentimos, no puedes publicar esta página.","unpublish":"Despublicar","unpublished":"La página se ha despublicado y ya no se puede acceder a ella.","publishing_settings":"Ajustes de publicación"},"change_owner":{"title":"Cambiar dueño","action":"cambiar dueño","error":"Se produjo un error al cambiar la autoría de las publicaciones.","placeholder":"nombre de usuario del nuevo dueño","instructions":{"one":"Por favor escoge el nuevo dueño del post de \u003cb\u003e@%{old_user}\u003c/b\u003e","other":"Por favor, escoge el nuevo dueño para las %{count} publicaciones de \u003cb\u003e@%{old_user}\u003c/b\u003e"},"instructions_without_old_user":{"one":"Por favor, escoge un nuevo propietario para la publicación.","other":"Por favor, escoge un nuevo propietario para las %{count} publicaciones"}},"change_timestamp":{"title":"Cambiar marca horaria...","action":"cambiar marca horaria","invalid_timestamp":"La marca horaria no puede ser en el futuro","error":"Hubo un error al cambiar la marca horaria de este tema.","instructions":"Por favor, selecciona la nueva marca horaria del tema. Las publicaciones en el tema se actualizarán para mantener la diferencia de tiempo."},"multi_select":{"select":"seleccionar","selected":"seleccionados (%{count})","select_post":{"label":"seleccionar","title":"Agregar publicación a la selección"},"selected_post":{"label":"seleccionado","title":"Haz clic para quitar publicaciones de la selección"},"select_replies":{"label":"seleccionar +respuestas","title":"Agregar publicación y todas sus respuestas a la selección"},"select_below":{"label":"seleccionar +abajo","title":"Agregar publicación y todo lo que le sigue a la selección"},"delete":"eliminar selección","cancel":"cancelar selección","select_all":"seleccionar todo","deselect_all":"deshacer seleccionar todo","description":{"one":"Has seleccionado \u003cb\u003e%{count}\u003c/b\u003e publicación.","other":"Has seleccionado \u003cb\u003e%{count}\u003c/b\u003e publicaciones."}},"deleted_by_author":{"one":"(tema retirado por su autor, se borrará automáticamente en %{count} hora, salvo que sea reportado)","other":"(tema retirado por el autor, se eliminará automáticamente en %{count} horas a menos de que sea reportado)"}},"post":{"quote_reply":"Citar","quote_share":"Compartir","edit_reason":"Motivo:","post_number":"publicación %{number}","ignored":"Contenido ignorado","reply_as_new_topic":"Responder como tema enlazado","reply_as_new_private_message":"Responder como mensaje nuevo a los mismos destinatarios","continue_discussion":"Continuando la discusión desde %{postLink}:","follow_quote":"ir a la publicación citada","show_full":"Mostrar la publicación completa","show_hidden":"Ver contenido ignorado.","deleted_by_author":{"one":"(post retirado por el autor. Será borrado automáticamente en %{count} hora si no es reportado)","other":"(publicación retirada por el autor, se eliminará automáticamente en %{count} horas a menos de que sea reportada)"},"collapse":"contraer","expand_collapse":"expandir/contraer","locked":"un miembro del staff bloqueó la posibilidad de editar esta publicación","gap":{"one":"ver %{count} post oculto","other":"ver %{count} publicaciones ocultas"},"notice":{"new_user":"Esta es la primera vez que %{user} ha publicado — ¡démosle la bienvenida a nuestra comunidad!","returning_user":"Hacía tiempo que no veíamos a %{user}, su última publicación fue %{time}."},"unread":"Publicaciones sin leer","has_replies":{"one":"%{count} Respuesta","other":"%{count} Respuestas"},"has_replies_count":"%{count}","unknown_user":"(desconocido/usuario eliminado)","has_likes_title":{"one":"%{count} persona le ha dado Me gusta a este post","other":"%{count} personas le han dado me gusta a esta publicación"},"has_likes_title_only_you":"te ha gustado este mensaje","has_likes_title_you":{"one":"A tí y a una persona le ha gustado este mensaje","other":"tú y a otros %{count} les han gustado este mensaje"},"filtered_replies_hint":{"one":"Ver esta publicación y su respuesta","other":"Ver esta publicación y sus %{count} respuestas"},"filtered_replies_viewing":{"one":"Viendo %{count} respuesta","other":"Viendo %{count} respuestas"},"in_reply_to":"Cargar publicación principal","errors":{"create":"Lo sentimos, se produjo un error al crear tu publicación. Por favor, inténtalo de nuevo.","edit":"Lo sentimos, se produjo un error al editar tu publicación. Por favor, inténtalo de nuevo.","upload":"Lo sentimos, se produjo un error al subir este archivo. Por favor, inténtalo de nuevo.","file_too_large":"Lo sentimos, ese archivo es demasiado grande (el tamaño máximo es %{max_size_kb} kb). ¿Por qué no lo subes a un servicio de almacenamiento en la nube y compartes el enlace luego?","too_many_uploads":"Lo sentimos, solo puedes subir un archivo a la vez.","too_many_dragged_and_dropped_files":{"one":"Solo se puede subir archivos de %{count} en 1.","other":"Solo se pueden subir %{count} archivos cada vez."},"upload_not_authorized":"Lo sentimos, el archivo que estás intentando subir no está permitido (extensiones autorizadas: %{authorized_extensions}).","image_upload_not_allowed_for_new_user":"Lo sentimos, los usuarios nuevos no pueden subir imágenes.","attachment_upload_not_allowed_for_new_user":"Lo sentimos, los usuarios nuevos no pueden subir archivos adjuntos.","attachment_download_requires_login":"Lo sentimos, necesitas haber iniciado sesión para descargar archivos adjuntos."},"abandon_edit":{"confirm":"¿Estás seguro de que quieres descartar tus cambios?","no_value":"No, permanecer","no_save_draft":"No, guardar borrador","yes_value":"Sí, descartar edición"},"abandon":{"title":"Abandonar borrador","confirm":"¿Estás seguro de que deseas abandonar tu publicación?","no_value":"No, permanecer","no_save_draft":"No, guardar borrador","yes_value":"Sí, abandonar"},"via_email":"esta publicación llegó por correo electrónico","via_auto_generated_email":"esta publicación llegó a través de un correo electrónico generado automáticamente","whisper":"esta publicación es un susurro privado para los moderadores","wiki":{"about":"esta publicación tiene formato wiki"},"archetypes":{"save":"Guardar opciones"},"few_likes_left":"¡Gracias por compartir amor! Solo te quedan unos pocos me gusta por hoy.","controls":{"reply":"crear una respuesta a esta publicación","like":"me gusta esta publicación","has_liked":"te gusta esta publicación","read_indicator":"miembros que han leído esta publicación","undo_like":"deshacer me gusta","edit":"editar esta publicación","edit_action":"Editar","edit_anonymous":"Lo sentimos, necesitas iniciar sesión para editar esta publicación.","flag":"reporta esta publicación de forma privada para llamar la atención de los moderadores o enviarles un notificación privada sobre el tema","delete":"eliminar este publicación","undelete":"deshacer la eliminación de esta publicación","share":"comparte un enlace a esta publicación","more":"Más","delete_replies":{"confirm":"¿Quieres eliminar también las respuestas a esta publicación?","direct_replies":{"one":"Si, y %{count} respuesta directa","other":"Sí, y las %{count} respuestas directas"},"all_replies":{"one":"Sí, y %{count} respuesta","other":"Sí, y todas las %{count} respuestas"},"just_the_post":"No, solo esta publicación"},"admin":"acciones de administrador para la publicación","wiki":"Transformar en formato wiki","unwiki":"Deshacer formato wiki","convert_to_moderator":"Convertir en publicación del staff","revert_to_regular":"Revertir el formato de publicación del staff","rebake":"Reconstruir HTML","publish_page":"Publicación de página","unhide":"Deshacer ocultar","change_owner":"Cambiar dueño","grant_badge":"Conceder medalla","lock_post":"Bloquear publicación","lock_post_description":"impedir que el usuario que realizó esta publicación la edite","unlock_post":"Desbloquear publicación","unlock_post_description":"permitir que el usuario que realizó esta publicación la edite","delete_topic_disallowed_modal":"No tienes permiso para eliminar este tema. Si de verdad quieres que se elimine, repórtalo y explica tus motivos a los moderadores.","delete_topic_disallowed":"no tienes permiso para eliminar este tema","delete_topic_confirm_modal_yes":"Sí, eliminar este tema","delete_topic_confirm_modal_no":"No, mantener este tema","delete_topic_error":"Ha ocurrido un error al eliminar este tema","delete_topic":"eliminar tema","add_post_notice":"Añadir aviso del staff","change_post_notice":"Cambiar aviso del staff","delete_post_notice":"Eliminar aviso del staff","remove_timer":"quitar temporizador"},"actions":{"people":{"like":{"one":"le dio me gusta a esto","other":"le dieron me gusta a esto"},"read":{"one":"leyó esto","other":"leyeron esto"},"like_capped":{"one":"y %{count} otro le gustó esto","other":"y %{count} otros le dieron me gusta a esto"},"read_capped":{"one":"y %{count} otro ha leído","other":"y %{count} otros han leído"}},"by_you":{"off_topic":"Reportaste esto como sin relación con el tema","spam":"Reportaste esto como spam","inappropriate":"Reportaste esto como inapropiado","notify_moderators":"Reportaste esto para que sea atendido por un moderador","notify_user":"Enviaste un mensaje a este usuario"}},"delete":{"confirm":{"one":"¿Estás seguro que quieres eliminar ese post?","other":"¿Estás seguro de que quieres eliminar estas %{count} publicaciones?"}},"merge":{"confirm":{"one":"Seguro que quieres unir esos posts?","other":"¿Estás seguro de que quieres fusionar estas %{count} publicaciones?"}},"revisions":{"controls":{"first":"Primera revisión","previous":"Revisión anterior","next":"Siguiente revisión","last":"Última revisión","hide":"Ocultar revisión.","show":"Mostrar revisión.","revert":"Volver a la revisión %{revision}","edit_wiki":"Editar wiki","edit_post":"Editar publicación","comparing_previous_to_current_out_of_total":"\u003cstrong\u003e%{previous}\u003c/strong\u003e %{icon} \u003cstrong\u003e%{current}\u003c/strong\u003e / %{total}"},"displays":{"inline":{"title":"Mostrar la producción renderizada con adiciones y eleminaciones en línea","button":"HTML"},"side_by_side":{"title":"Mostrar las differencias con la producción renderizada lado a lado","button":"HTML"},"side_by_side_markdown":{"title":"Mostrar las diferencias con la fuente sin procesar","button":"Sin procesar"}}},"raw_email":{"displays":{"raw":{"title":"Mostrar correos electrónicos sin procesar","button":"Sin procesar"},"text_part":{"title":"Mostrar la parte del texto del correo electrónico","button":"Texto"},"html_part":{"title":"Mostrar la parte HTML del correo electrónico","button":"HTML"}}},"bookmarks":{"create":"Crear marcador","edit":"Editar marcador","created":"Creado","updated":"Actualizado","name":"Nombre","name_placeholder":"¿Para qué es este marcador?","set_reminder":"Recuérdame","actions":{"delete_bookmark":{"name":"Eliminar marcador","description":"Elimina el marcador de tu perfil y cancela todos los recordatorios para tal marcador"},"edit_bookmark":{"name":"Editar marcador","description":"Editar el nombre del marcador o cambia la fecha y hora del recordatorio"}}},"filtered_replies":{"viewing_posts_by":"Viendo %{post_count} publicaciones de","viewing_subset":"Algunas respuestas no se muestran","viewing_summary":"Estás viendo un resumen de este tema","post_number":"%{username}, publicación nº %{post_number}","show_all":"Mostrar todas"}},"category":{"can":"puede\u0026hellip; ","none":"(sin categoría)","all":"Todas las categorías","choose":"categoría\u0026hellip;","edit":"Editar","edit_dialog_title":"Editar: %{categoryName}","view":"Ver temas en la categoría","back":"Volver a la categoría","general":"General","settings":"Ajustes","topic_template":"Plantilla de tema","tags":"Etiquetas","tags_allowed_tags":"Restringir estas etiquetas a esta categoría:","tags_allowed_tag_groups":"Restringir estos grupos de etiquetas a esta categoría:","tags_placeholder":"(Opcional) lista de etiquetas permitidas","tags_tab_description":"Las etiquetas y grupos de etiquetas arriba especificados solo estarán disponibles en esta categoría y en las otras categorías que igualmente lo especifiquen. No se permitirá su uso en otras categorías.","tag_groups_placeholder":"(Opcional) lista de grupos de etiquetas permitidos","manage_tag_groups_link":"Gestionar grupos de etiquetas","allow_global_tags_label":"Permitir también otras etiquetas","tag_group_selector_placeholder":"(Opcional) Grupo de etiquetas","required_tag_group_description":"Requerir que los nuevos temas tengan etiquetas de un grupo de etiquetas:","min_tags_from_required_group_label":"Número de etiquetas:","required_tag_group_label":"Grupo de etiquetas:","topic_featured_link_allowed":"Permitir enlaces destacados en esta categoría","delete":"Eliminar categoría","create":"Crear categoría","create_long":"Crear una nueva categoría","save":"Guardar categoría","slug":"Slug de la categoría para URL","slug_placeholder":"(Opcional) palabras-separadas-por-guiones para URL","creation_error":"Se produjo un error al crear la categoría.","save_error":"Se produjo un error al guardar la categoría","name":"Nombre de la categoría","description":"Descripción","topic":"tema de la categoría","logo":"Imagen (logo) para la categoría","background_image":"Imagen de fondo de la categoría","badge_colors":"Colores de las medallas","background_color":"Color de fondo","foreground_color":"Colores de primer plano","name_placeholder":"Una o dos palabras máximo","color_placeholder":"Cualquier color web","delete_confirm":"¿Estás seguro de que quieres eliminar esta categoría?","delete_error":"Se produjo un error al eliminar la categoría.","list":"Lista de categorías","no_description":"Por favor, agrega una descripción para esta categoría.","change_in_category_topic":"Editar descripción","already_used":"Este color ya ha sido usado para otra categoría","security":"Seguridad","security_add_group":"Añadir grupo","permissions":{"group":"Grupo","see":"Ver","reply":"Responder","create":"Crear","no_groups_selected":"Ningún grupo tiene acceso. Esta categoría solo será visible por el staff.","everyone_has_access":"Esta categoría es pública. Todo el mundo puede ver, responder y crear temas. Para restringir permisos, quita uno o varios permisos del grupo «todos».","toggle_reply":"Alternar el permiso para responder","toggle_full":"Alternar el permiso para crear temas","inherited":"Este permiso está heredado de «todos»"},"special_warning":"Aviso: esta categoría se ajusta por defecto y las opciones de seguridad no pueden ser editadas. Si no deseas utilizarla, elimínala en vez de reutilizarla.","uncategorized_security_warning":"Esta categoría es especial: se usa para temas que no tienen una categoría asignada y y no puede tener ajustes de seguridad.","uncategorized_general_warning":"Esta categoría es especial. Se utiliza como la categoría predeterminada para los temas nuevos que no tienen una categoría seleccionada. Si deseas evitar este comportamiento y forzar la selección de categorías, \u003ca href=\"%{settingLink}\"\u003epor favor, desactiva la opción aquí\u003c/a\u003e. Si deseas cambiar el nombre o la descripción, ve a \u003ca href=\"%{customizeLink}\"\u003ePersonalizar / Contenido de texto\u003c/a\u003e.","pending_permission_change_alert":"No has agregado a %{group} a esta categoría. Haz clic en este botón para agregarlos.","images":"Imágenes","email_in":"Dirección de correo electrónico personalizada para el correo entrante:","email_in_allow_strangers":"Aceptar correo electrónicos de usuarios anónimos sin cuenta","email_in_disabled":"La posibilidad de publicar temas nuevos por correo electrónico está deshabilitada en los ajustes del sitio. Para habilitar la publicación de temas nuevos por correo electrónico,","email_in_disabled_click":"activa la opción «correo electrónico»","mailinglist_mirror":"La categoría es el reflejo de una lista de correo","show_subcategory_list":"Mostrar la lista de subcategorías arriba de la lista de temas en esta categoría.","read_only_banner":"Texto de encabezado cuando un usuario no pueda crear un tema en esta categoría:","num_featured_topics":"Número de temas que se muestran en la página de categorías:","subcategory_num_featured_topics":"Número de temas destacados a mostrar en la página principal de categorías:","all_topics_wiki":"Hacer que los temas nuevos tengan formato wiki por defecto","subcategory_list_style":"Estilo de lista de subcategorías:","sort_order":"Ordenar lista de temas:","default_view":"Orden por defecto:","default_top_period":"Período por defecto para estar en la parte superior:","default_list_filter":"Filtro de lista por defecto:","allow_badges_label":"Permitir que se concedan medallas en esta categoría","edit_permissions":"Editar permisos","reviewable_by_group":"Además del staff, el contenido de esta categoría también puede ser revisado por:","review_group_name":"nombre del grupo","require_topic_approval":"Requiere aprobación del moderador para todos los temas nuevos","require_reply_approval":"Requiere aprobación del moderador para todas las respuestas nuevas","this_year":"este año","position":"Posición en la página de categorías:","default_position":"Posición predeterminada","position_disabled":"Las Categorías se mostrarán por orden de actividad. Para controlar el orden en que aparecen en las listas,","position_disabled_click":"activa la opción «posiciones de categoría fijas».","minimum_required_tags":"Número mínimo de etiquetas requeridas en un tema:","parent":"Categoría primaria","num_auto_bump_daily":"Número de temas abiertos a los que se le hará bump de diariamente de forma automática:","navigate_to_first_post_after_read":"Ir a la primera publicación después de haber leído los temas","notifications":{"watching":{"title":"Vigilar"},"watching_first_post":{"title":"Vigilar la primera publicación","description":"Se te notificará acerca de los temas nuevos en esta categoría, pero no cuando haya respuestas nuevas a los temas."},"tracking":{"title":"Seguir"},"regular":{"title":"Normal","description":"Se te notificará solo si alguien menciona tu @nombre o te responde."},"muted":{"title":"Silenciar"}},"search_priority":{"label":"Prioridad de búsqueda","options":{"normal":"Normal","ignore":"Ignorar","very_low":"Muy baja","low":"Baja","high":"Alta","very_high":"Muy alta"}},"sort_options":{"default":"por defecto","likes":"Me gusta","op_likes":"Me gusta de la publicación original","views":"Visitas","posts":"Publicaciones","activity":"Actividad","posters":"Participantes","category":"Categoría","created":"Creado"},"sort_ascending":"Ascendente","sort_descending":"Descendente","subcategory_list_styles":{"rows":"Filas","rows_with_featured_topics":"Filas con temas destacados","boxes":"Cajas","boxes_with_featured_topics":"Cajas con temas destacados"},"settings_sections":{"general":"General","moderation":"Moderación","appearance":"Aspecto","email":"Correo electrónico"},"list_filters":{"all":"todos los temas","none":"sin subcategoría"},"colors_disabled":"No puedes seleccionar colores porque tienes no tienes activados los estilos de categoría."},"flagging":{"title":"¡Gracias por ayudar a mantener una comunidad civilizada!","action":"Reportar publicación","take_action":"Tomar medidas...","take_action_options":{"default":{"title":"Tomar medidas","details":"Alcanzar el umbral de reportes inmediatamente en vez de esperar más reportes de la comunidad"},"suspend":{"title":"Suspender usuario","details":"Alcanzar el umbral del reporte y suspender al usuario"},"silence":{"title":"Silenciar usuario","details":"Alcanzar el umbral del reporte y silenciar al usuario"}},"notify_action":"Mensaje","official_warning":"Advertencia oficial","delete_spammer":"Eliminar spammer","yes_delete_spammer":"Sí, eliminar spammer","ip_address_missing":"(N/D)","hidden_email_address":"(oculto)","submit_tooltip":"Enviar el reporte privado","take_action_tooltip":"Alcanzar el umbral de reportes inmediatamente en vez de esperar más reportes de la comunidad","cant":"Lo sentimos, no puedes reportar esta publicación en este momento.","notify_staff":"Notificar a los administradores de forma privada","formatted_name":{"off_topic":"No tiene relación con el tema","inappropriate":"Es inapropiado","spam":"Es spam"},"custom_placeholder_notify_user":"Sé específico, constructivo y siempre amable.","custom_placeholder_notify_moderators":"Haznos saber qué te preocupa específicamente y, siempre que sea posible, incluye enlaces y ejemplos relevantes.","custom_message":{"at_least":{"one":"introduce al menos %{count} caracteres","other":"ingresa al menos %{count} caracteres"},"more":{"one":"%{count} más...","other":"%{count} más..."},"left":{"one":"%{count} restante","other":"%{count} restantes"}}},"flagging_topic":{"title":"¡Gracias por ayudar a mantener una comunidad civilizada!","action":"Reportar tema","notify_action":"Mensaje"},"topic_map":{"title":"Resumen de temas","participants_title":"Autores frecuentes","links_title":"Enlaces populares","links_shown":"mostrar más enlaces...","clicks":{"one":"%{count} clic","other":"%{count} clics"}},"post_links":{"about":"ver más enlaces de esta publicación","title":{"one":"%{count} más","other":"%{count} más"}},"topic_statuses":{"warning":{"help":"Ésta es una advertencia oficial."},"bookmarked":{"help":"Guardaste en marcadores este tema"},"locked":{"help":"Este tema está cerrado; ya no se aceptan respuestas nuevas"},"archived":{"help":"este tema está archivado; está congelado y no se puede cambiar"},"locked_and_archived":{"help":"este tema está cerrado y archivado; ya no acepta respuestas nuevas y no se puede cambiar"},"unpinned":{"title":"Dejar de destacar","help":"Este tema se ha dejado de destacar para ti; tu lista de temas se mostrará en orden normal"},"pinned_globally":{"title":"Destacado globalmente","help":"Este tema ha sido destacado globalmente, se mostrará en la parte superior de la página de mensajes recientes y de su categoría."},"pinned":{"title":"Destacado","help":"Este tema ha sido destacado para ti; se mostrará en la parte superior de su categoría"},"unlisted":{"help":"Este tema es invisible. No se mostrará en la lista de temas y solo se le puede acceder mediante un enlace directo"},"personal_message":{"title":"Este tema es un mensaje personal","help":"Este tema es un mensaje personal"},"solved":{"help":"Este tema tiene una solución"}},"posts":"Publicaciones","original_post":"Publicación original","views":"Vistas","views_lowercase":{"one":"visita","other":"vistas"},"replies":"Respuestas","views_long":{"one":"este tema se ha visto %{count} vez","other":"este tema se ha visto %{number} veces"},"activity":"Actividad","likes":"Me gusta","likes_lowercase":{"one":"me gusta","other":"me gusta"},"users":"Usuarios","users_lowercase":{"one":"usuario","other":"usuarios"},"category_title":"Categoría","history":"Historial","changed_by":"por %{author}","raw_email":{"title":"Correo electrónicos entrantes","not_available":"¡No disponible!"},"categories_list":"Lista de categorías","filters":{"with_topics":"%{filter} temas","with_category":"%{filter} Foro de %{category}","latest":{"title":"Recientes","title_with_count":{"one":"Reciente (%{count})","other":"Recientes (%{count})"},"help":"temas con publicaciones recientes"},"read":{"title":"Leídos","help":"temas que ya has leído en el orden que los leíste por última vez"},"categories":{"title":"Categorías","title_in":"Categoría - %{categoryName}","help":"todos los temas agrupados por categoría"},"unread":{"title":"Sin leer","title_with_count":{"one":"Sin leer (%{count})","other":"Sin leer (%{count})"},"help":"temas que estás vigilando o siguiendo actualmente con publicaciones sin leer","lower_title_with_count":{"one":"%{count} sin leer","other":"%{count} sin leer"}},"new":{"lower_title_with_count":{"one":"%{count} tema nuevo","other":"%{count} nuevos"},"lower_title":"nuevo","title":"Nuevo","title_with_count":{"one":"Nuevos (%{count})","other":"Nuevos (%{count})"},"help":"temas creados en los últimos días"},"posted":{"title":"Mis publicaciones","help":"temas en los que has publicado"},"bookmarks":{"title":"Marcadores","help":"temas que has guardado en marcadores"},"category":{"title":"%{categoryName}","title_with_count":{"one":"%{categoryName} (%{count})","other":"%{categoryName} (%{count})"},"help":"temas recientes en la categoría %{categoryName}"},"top":{"title":"Destacados","help":"los temas con más actividad en el último año, mes, semana o día","all":{"title":"Siempre"},"yearly":{"title":"Anualmente"},"quarterly":{"title":"Trimestralmente"},"monthly":{"title":"Mensualmente"},"weekly":{"title":"Semanalmente"},"daily":{"title":"Diariamente"},"all_time":"Siempre","this_year":"Año","this_quarter":"Trimestre","this_month":"Mes","this_week":"Semana","today":"Hoy","other_periods":"ver arriba:"}},"browser_update":"Lamentablemente, \u003ca href=\"https://www.discourse.org/faq/#browser\"\u003etu navegador es demasiado antiguo para funcionar en este sitio\u003c/a\u003e. Por favor, \u003ca href=\"https://browsehappy.com\"\u003eactualiza tu navegador\u003c/a\u003e para ver el contenido enriquecido, iniciar sesión y responder.","permission_types":{"full":"Crear / Responder / Ver","create_post":"Responder / Ver","readonly":"Ver"},"lightbox":{"download":"descargar","previous":"Anterior (flecha izquierda)","next":"Siguiente (flecha derecha)","counter":"%curr% de %total%","close":"Cerrar (Esc)","content_load_error":"\u003ca href=\"%url%\"\u003eEl contenido\u003c/a\u003e no se pudo cargar.","image_load_error":"\u003ca href=\"%url%\"\u003eLa imagen\u003c/a\u003e no se pudo cargar."},"keyboard_shortcuts_help":{"shortcut_key_delimiter_comma":",","shortcut_key_delimiter_plus":"+","shortcut_delimiter_or":"%{shortcut1} o %{shortcut2}","shortcut_delimiter_slash":"%{shortcut1}/%{shortcut2}","shortcut_delimiter_space":"%{shortcut1} %{shortcut2}","title":"Atajos de teclado","jump_to":{"title":"Saltar a","home":"%{shortcut} Inicio","latest":"%{shortcut} Recientes","new":"%{shortcut} Nuevos","unread":"%{shortcut} Sin leer","categories":"%{shortcut} Categorías","top":"%{shortcut} Destacado","bookmarks":"%{shortcut} Marcadores","profile":"%{shortcut} Perfil","messages":"%{shortcut} Mensajes","drafts":"%{shortcut} Borradores","next":"%{shortcut} Tema siguiente","previous":"%{shortcut} Tema anterior"},"navigation":{"title":"Navegación","jump":"%{shortcut} Ir a la publicación #","back":"%{shortcut} Volver","up_down":"%{shortcut} Desplazar selección \u0026uarr; \u0026darr;","open":"%{shortcut} Abrir tema seleccionado","next_prev":"%{shortcut} Sección siguiente/anterior","go_to_unread_post":"%{shortcut} Ir a la primera publicación sin leer"},"application":{"title":"Aplicación","create":"%{shortcut} Crear un nuevo tema","notifications":"%{shortcut} Abrir notificaciones","hamburger_menu":"%{shortcut} Abrir menú hamburguesa","user_profile_menu":"%{shortcut} Abrir menú de usuario","show_incoming_updated_topics":"%{shortcut} Mostrar temas actualizados","search":"%{shortcut} Buscar","help":"%{shortcut} Abrir guía de atajos de teclado","dismiss_new_posts":"%{shortcut} Descartar publicaciones nuevas","dismiss_topics":"%{shortcut} Descartar temas","log_out":"%{shortcut} Cerrar sesión"},"composing":{"title":"Redactando","return":"%{shortcut} Regresar al editor","fullscreen":"%{shortcut} Edición en pantalla completa"},"bookmarks":{"title":"Marcadores","enter":"%{shortcut} Guardar y cerrar","later_today":"%{shortcut} Hoy más tarde","later_this_week":"%{shortcut} Esta semana","tomorrow":"%{shortcut} Mañana","next_week":"%{shortcut} La semana que viene","next_month":"%{shortcut} El mes que viene","next_business_week":"%{shortcut} Principio de la semana que viene","next_business_day":"%{shortcut} Siguiente día hábil","custom":"%{shortcut} Fecha y hora personalizadas","none":"%{shortcut} Sin recordatorio","delete":"%{shortcut} Borrar marcador"},"actions":{"title":"Acciones","bookmark_topic":"%{shortcut} Guardar/quitar el tema de marcadores","pin_unpin_topic":"%{shortcut} Destacar/dejar de destacar tema","share_topic":"%{shortcut} Compartir tema","share_post":"%{shortcut} Compartir publicación","reply_as_new_topic":"%{shortcut} Responder como un tema enlazado","reply_topic":"%{shortcut} Responder al tema","reply_post":"%{shortcut} Responder a la publicación","quote_post":"%{shortcut} Citar publicación","like":"%{shortcut} Me gusta la publicación","flag":"%{shortcut} Reportar publicación","bookmark":"%{shortcut} Guardar publicación en marcadores","edit":"%{shortcut} Editar publicación","delete":"%{shortcut} Eliminar publicación","mark_muted":"%{shortcut} Silenciar tema","mark_regular":"%{shortcut} Seguimiento normal del tema normal (por defecto)","mark_tracking":"%{shortcut} Seguir tema","mark_watching":"%{shortcut} Vigilar Tema","print":"%{shortcut} Imprimir tema","defer":"%{shortcut} Aplazar el tema","topic_admin_actions":"%{shortcut} Abrir acciones de administrador del tema"},"search_menu":{"title":"Buscar Menu","prev_next":"%{shortcut} Mover selección arriba y abajo","insert_url":"%{shortcut} Insertar selección dentro del editor abierto"}},"badges":{"earned_n_times":{"one":"Ganó esta medalla %{count} vez","other":"Medalla ganada %{count} veces"},"granted_on":"Concedido hace %{date}","others_count":"Otras personas con esta medalla (%{count})","title":"Medallas","allow_title":"Puedes usar esta medalla como título","multiple_grant":"Puedes ganar esta medalla varias veces","badge_count":{"one":"%{count} medalla","other":"%{count} medallas"},"more_badges":{"one":"+%{count} Más","other":"+%{count} Más"},"granted":{"one":"%{count} concedido","other":"%{count} concedidas"},"select_badge_for_title":"Selecciona una medalla para utilizar como tu título","none":"(ninguna)","successfully_granted":"%{badge} concedida exitosamente a %{username}","badge_grouping":{"getting_started":{"name":"Guía de inicio"},"community":{"name":"Comunidad"},"trust_level":{"name":"Nivel de confianza"},"other":{"name":"Miscelánea"},"posting":{"name":"Publicación"}}},"tagging":{"all_tags":"Todas las etiquetas","other_tags":"Otras etiquetas","selector_all_tags":"todas las etiquetas","selector_no_tags":"sin etiquetas","changed":"etiquetas cambiadas:","tags":"Etiquetas","choose_for_topic":"etiquetas opcionales","info":"Info","default_info":"Esta etiqueta no está restringida a ninguna categoría, y no tiene sinónimos.","category_restricted":"Esta etiqueta está restringida para las categorías a las que no tienes permiso de acceso.","synonyms":"Sinónimos","synonyms_description":"Cuando las siguientes etiquetas sean usadas, serán reemplazadas por \u003cb\u003e%{base_tag_name}\u003c/b\u003e.","tag_groups_info":{"one":"Esta etiqueta pertence al grupo: «%{tag_groups}»","other":"Esta etiqueta pertence a estos grupos: %{tag_groups}."},"category_restrictions":{"one":"Solo se puede utilizar en esta categoría:","other":"Solo se puede utilizar en estas categorías:"},"edit_synonyms":"Gestionar sinónimos","add_synonyms_label":"Añadir sinónimos:","add_synonyms":"Añadir","add_synonyms_explanation":{"one":"Se cambiarán la etiqueta en cualquier sitio que esté en uso, sustituyéndose por \u003cb\u003e%{tag_name}\u003c/b\u003e en su lugar. ¿Seguro que quieres hacer el cambio?","other":"Se cambiarán las etiquetas en todos los lugares en los que estén en uso y se sustituirán por \u003cb\u003e%{tag_name}\u003c/b\u003e en su lugar. ¿Seguro de que quieres hacer este cambio?"},"add_synonyms_failed":"No se han podido añadir las siguientes etiquetas como sinónimos: \u003cb\u003e%{tag_names}\u003c/b\u003e. Asegúrate de que no tienen sinónimos y de que no son sinónimos de otra etiqueta.","remove_synonym":"Quitar sinónimo","delete_synonym_confirm":"¿Estás seguro de que quieres eliminar el sinónimo «%{tag_name}»?","delete_tag":"Eliminar etiqueta","delete_confirm":{"one":"¿Estás seguro de querer borrar esta etiqueta y eliminarla de %{count} tema asignado?","other":"¿Estás seguro de que quieres eliminar esta etiqueta y quitarla de los %{count} temas a los que está asignada?"},"delete_confirm_no_topics":"¿Estás seguro de que quieres eliminar esta etiqueta?","delete_confirm_synonyms":{"one":"Su sinónimo también se eliminará","other":"Sus %{count} sinónimos también se eliminarán."},"rename_tag":"Renombrar etiqueta","rename_instructions":"Elige un nuevo nombre para la etiqueta:","sort_by":"Ordenar por:","sort_by_count":"contador","sort_by_name":"nombre","manage_groups":"Administrar grupos de etiquetas","manage_groups_description":"Definir grupos para organizar etiquetas","upload":"Subir etiquetas","upload_description":"Sube un archivo csv para crear etiquetas en masa","upload_instructions":"Una por línea, opcional con un grupo de etiquetas en el formato «tag_name,tag_group».","upload_successful":"Etiquetas subidas con éxito","delete_unused_confirmation":{"one":"%{count} etiqueta será eliminada: %{tags}","other":"%{count} etiquetas serán eliminadas: %{tags}"},"delete_unused_confirmation_more_tags":{"one":"%{tags} y %{count} más","other":"%{tags} y %{count} más"},"delete_no_unused_tags":"No hay etiquetas sin usar.","delete_unused":"Eliminar etiquetas sin usar","delete_unused_description":"Eliminar todas las etiquetas que no estén asociadas a ningún tema o mensaje personal","cancel_delete_unused":"Cancelar","filters":{"without_category":"%{filter} %{tag} temas","with_category":"%{filter} %{tag} temas en %{category}","untagged_without_category":"%{filter} temas sin etiquetas","untagged_with_category":"%{filter} temas sin etiquetas en %{category}"},"notifications":{"watching":{"title":"Vigilar","description":"Vigilarás automáticamente todos los temas con esta etiqueta. Se te notificarán todos los temas y publicaciones nuevas. Además aparecerá un contador de publicaciones nuevas y sin leer al lado del tema."},"watching_first_post":{"title":"Vigilar primera publicación","description":"Se te notificará acerca de nuevos temas con esta etiqueta, pero no cuando haya respuestas al tema."},"tracking":{"title":"Seguir","description":"Seguirás automáticamente todos los temas con esta etiqueta. Aparecerá un contador de publicaciones nuevas y sin leer al lado del tema."},"regular":{"title":"Normal","description":"Se te notificará si alguien menciona tu @nombre o responde a alguna de tus publicaciones."},"muted":{"title":"Silenciado","description":"No se te notificará sobre temas nuevos con esta etiqueta, ni aparecerán en tu pestaña de no leídos."}},"groups":{"title":"Grupos de etiquetas","about":"Agrupar etiquetas para administrarlas más fácilmente.","new":"Nuevo grupo","tags_label":"Etiquetas en este grupo:","parent_tag_label":"Etiqueta primaria:","parent_tag_description":"Las etiquetas de este grupo no se pueden utilizar a menos que la etiqueta primaria esté presente. ","one_per_topic_label":"Limitar las etiquetas de este grupo a utilizarse solo una vez por tema","new_name":"Nuevo grupo de etiquetas","name_placeholder":"Nombre del grupo de etiquetas","save":"Guardar","delete":"Eliminar","confirm_delete":"¿Estás seguro de que quieres eliminar este grupo de etiquetas?","everyone_can_use":"Todos pueden utilizar las etiquetas","usable_only_by_groups":"Las etiquetas son visibles para todo el mundo, pero solo los siguientes grupos las pueden usar","visible_only_to_groups":"Las etiquetas sólo son visibles para los siguientes grupos"},"topics":{"none":{"unread":"No tienes temas sin leer.","new":"No tienes temas nuevos.","read":"Todavía no has leído ningún tema.","posted":"Todavía no has publicado en ningún tema.","latest":"No hay temas recientes.","bookmarks":"No has guardado ningún tema en marcadores todavía.","top":"No hay temas destacados."}}},"invite":{"custom_message":"Dale a tu invitación un toque personal escribiendo un \u003ca href\u003emensaje personalizado\u003c/a\u003e.","custom_message_placeholder":"Ingresa tu mensaje personalizado","approval_not_required":"El usuario será aprobado automáticamente en cuanto acepte esta invitación.","custom_message_template_forum":"¡Hey, deberías unirte a este foro!","custom_message_template_topic":"¡Hey, creemos que este tema te va a encantar!"},"forced_anonymous":"Debido a una carga extrema, esto se está mostrando temporalmente a todos como lo vería un usuario que no haya iniciado sesión.","footer_nav":{"back":"Atrás","forward":"Avanzar","share":"Compartir","dismiss":"Descartar"},"safe_mode":{"enabled":"El modo seguro está activado, para salir del modo seguro cierra esta ventana del navegador"},"image_removed":"(imagen eliminada)","do_not_disturb":{"title":"No molestar durante...","label":"No molestar","remaining":"%{remaining} restante","options":{"half_hour":"30 minutos","one_hour":"1 hora","two_hours":"2 horas","tomorrow":"Hasta mañana","custom":"Personalizado"},"set_schedule":"Establecer un horario de notificaciones"},"presence":{"replying":{"one":"respondiendo","other":"respondiendo"},"editing":{"one":"editando","other":"editando"},"replying_to_topic":{"one":"respondiendo","other":"respondiendo"}},"poll":{"voters":{"one":"votante","other":"votantes"},"total_votes":{"one":"voto total","other":"total de votos"},"average_rating":"Puntuación media: \u003cstrong\u003e%{average}\u003c/strong\u003e.","public":{"title":"Los votos son \u003cstrong\u003epúblicos\u003c/strong\u003e."},"results":{"groups":{"title":"Necesitas ser parte del grupo %{groups} para votar en esta encuesta."},"vote":{"title":"Los resultados se mostrarán cuando \u003cstrong\u003evotes\u003c/strong\u003e."},"closed":{"title":"Los resultados se mostrarán una vez que la encuesta esté \u003cstrong\u003ecerrada\u003c/strong\u003e."},"staff":{"title":"Los resultados solo se muestran a miembros del \u003cstrong\u003estaff\u003c/strong\u003e."}},"multiple":{"help":{"at_least_min_options":{"one":"Selecciona al menos \u003cstrong\u003e%{count}\u003c/strong\u003e opción.","other":"Selecciona al menos \u003cstrong\u003e%{count}\u003c/strong\u003e opciones."},"up_to_max_options":{"one":"Selecciona como máximo \u003cstrong\u003e%{count}\u003c/strong\u003e opción.","other":"Selecciona como máximo \u003cstrong\u003e%{count}\u003c/strong\u003e opciones."},"x_options":{"one":"Selecciona \u003cstrong\u003e%{count}\u003c/strong\u003e opción.","other":"Selecciona \u003cstrong\u003e%{count}\u003c/strong\u003e opciones."},"between_min_and_max_options":"Selecciona de \u003cstrong\u003e%{min}\u003c/strong\u003e a \u003cstrong\u003e%{max}\u003c/strong\u003e opciones."}},"cast-votes":{"title":"Emite tus votos","label":"¡Vota ahora!"},"show-results":{"title":"Mostrar los resultados de la encuesta","label":"Mostrar resultados"},"hide-results":{"title":"Volver a tus votos","label":"Mostrar votos"},"group-results":{"title":"Agrupar votos por campo de usuario","label":"Mostrar desglose"},"export-results":{"title":"Exportar los resultados de la encuesta","label":"Exportar"},"open":{"title":"Abrir la encuesta","label":"Abrir","confirm":"¿Estás seguro de que quieres abrir esta encuesta?"},"close":{"title":"Cerrar la encuesta","label":"Cerrar","confirm":"¿Estás seguro de que quieres cerrar esta encuesta?"},"automatic_close":{"closes_in":"Cierra en \u003cstrong\u003e%{timeLeft}\u003c/strong\u003e.","age":"Cerrado \u003cstrong\u003e%{age}\u003c/strong\u003e"},"breakdown":{"title":"Resultados de la encuesta","votes":"%{count} votos","breakdown":"Desglose","percentage":"Porcentaje","count":"Contar"},"error_while_toggling_status":"Lo sentimos, se produjo un error al cambiar el estado de esta encuesta.","error_while_casting_votes":"Lo sentimos, se produjo un error al emitir tus votos.","error_while_fetching_voters":"Lo sentimos, se produjo un error al mostrar los votantes.","error_while_exporting_results":"Lo sentimos, ha habido un error al exportar los resultados de la encuesta.","ui_builder":{"title":"Crear encuesta","insert":"Insertar encuesta","help":{"options_count":"Ingresa al menos 1 opción","invalid_values":"El valor mínimo debe ser menor que el valor máximo.","min_step_value":"El valor mínimo es 1"},"poll_type":{"label":"Tipo","regular":"Una opción","multiple":"Múltiples opciones","number":"Valoración numérica"},"poll_result":{"label":"Resultados","always":"Siempre visible","vote":"En votación","closed":"Cuando esté cerrada","staff":"Solo staff"},"poll_groups":{"label":"Grupos permitidos"},"poll_chart_type":{"label":"Tipo de gráfico","bar":"Barras","pie":"Tarta"},"poll_config":{"max":"Máximo","min":"Mínimo","step":"Intervalo"},"poll_public":{"label":"Mostrar quién votó"},"poll_title":{"label":"Título (opcional)"},"poll_options":{"label":"Ingresa una opción de la encuesta por línea"},"automatic_close":{"label":"Cerrar encuesta automáticamente"}}},"discourse_local_dates":{"relative_dates":{"today":"Hoy %{time}","tomorrow":"Mañana %{time}","yesterday":"Ayer %{time}","countdown":{"passed":"la fecha ha pasado"}},"title":"insertar fecha / hora","create":{"form":{"insert":"Insertar","advanced_mode":"Modo avanzado","simple_mode":"Modo simple","timezones_title":"Zonas horarias que se muestran","timezones_description":"Las zonas horarias se usarán para mostrar las fechas en vista previa y retrospectiva.","recurring_title":"Recurrencia","recurring_description":"Definir la recurrencia de un evento. También puedes editar manualmente la opción recurrente generada por el formulario y usar una de las siguientes claves: años, trimestre, meses, semanas, días, horas, minutos, segundos, milisegundos.","recurring_none":"Sin recurrencia","invalid_date":"Fecha inválida, asegúrate de que la fecha y hora sean correctas","date_title":"Fecha","time_title":"Hora","format_title":"Formato de fecha","timezone":"Zona horaria","until":"Hasta...","recurring":{"every_day":"Cada día","every_week":"Cada semana","every_two_weeks":"Cada dos semanas","every_month":"Cada mes","every_two_months":"Cada dos meses","every_three_months":"Cada tres meses","every_six_months":"Cada seis meses","every_year":"Cada año"}}}},"styleguide":{"title":"Guía de estilos","welcome":"Para empezar, elige una sección en el menú de la izquierda.","categories":{"atoms":"Átomos","molecules":"Moléculas","organisms":"Organismos"},"sections":{"typography":{"title":"Tipografía","example":"Te damos la bienvenida a Discourse","paragraph":"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."},"date_time_inputs":{"title":"Campos de entrada de fecha/hora"},"font_scale":{"title":"Sistema de fuentes"},"colors":{"title":"Colores"},"icons":{"title":"Iconos","full_list":"Ver la lista completa de iconos de Font Awesome"},"input_fields":{"title":"Campos de entrada de datos"},"buttons":{"title":"Botones"},"dropdowns":{"title":"Menús desplegables"},"categories":{"title":"Categorías"},"bread_crumbs":{"title":"Migas de pan"},"navigation":{"title":"Navegación"},"navigation_bar":{"title":"Barra de navegación"},"navigation_stacked":{"title":"Navegación apilada"},"categories_list":{"title":"Lista de categorías"},"topic_link":{"title":"Tema de enlace"},"topic_list_item":{"title":"Elemento de la lista de temas"},"topic_statuses":{"title":"Estados de los temas"},"topic_list":{"title":"Lista de temas"},"basic_topic_list":{"title":"Lista básica de temas"},"footer_message":{"title":"Mensaje de pie de página"},"signup_cta":{"title":"CTA de registro"},"topic_timer_info":{"title":"Temporizadores de temas"},"topic_footer_buttons":{"title":"Botones de pie de página de tema"},"topic_notifications":{"title":"Notificaciones del tema"},"post":{"title":"Publicación"},"topic_map":{"title":"Mapa del tema"},"site_header":{"title":"Encabezado del sitio"},"suggested_topics":{"title":"Temas sugeridos"},"post_menu":{"title":"Menú de la publicación"},"modal":{"title":"Ventana modal","header":"Título de la ventana modal","footer":"Pie de página de la ventana modal"},"user_about":{"title":"Caja de acerca de usuario"},"header_icons":{"title":"Iconos de encabezado"},"spinners":{"title":"Indicadores de carga"}}},"details":{"title":"Ocultar detalles"},"discourse_narrative_bot":{"welcome_post_type":{"new_user_track":"Inicia el tutorial de usuario nuevo para todos los usuarios nuevos","welcome_message":"Envía a todos los usuarios nuevos un mensaje de bienvenida junto a una guía de inicio rápido"}},"solved":{"title":"Solucionado","allow_accepted_answers":"Permitir al propietario del tema y al staff marcar una respuesta como la solución","accept_answer":"Selecciona si esta respuesta resuelve el problema","accepted_description":"Esta es la solución aceptada para este tema","has_no_accepted_answer":"Este tema no tiene una solución","unaccept_answer":"Deseleccionar si esta respuesta ya no resuelve el problema","accepted_answer":"Solución","solution":"Solución","solution_summary":{"one":"solución","other":"soluciones"},"accepted_html":"%{icon} Resuelto \u003cspan class='by'\u003epor \u003ca href data-user-card='%{username_lower}'\u003e%{username}\u003c/a\u003e\u003c/span\u003e en la \u003ca href='%{post_path}' class='back'\u003epublicación n° %{post_number}\u003c/a\u003e","accepted_notification":"\u003cp\u003e\u003cspan\u003e%{username}\u003c/span\u003e %{description}\u003c/p\u003e","topic_status_filter":{"all":"todos","solved":"solucionado","unsolved":"sin solución"}},"admin":{"web_hooks":{"solved_event":{"name":"Al resolver","details":"Cuando un usuario marca o desmarca una publicación como la respuesta aceptada."}},"logs":{"staff_actions":{"actions":{"discourse_upgrade":"Actualizar a la última versión"}}},"site_settings":{"categories":{"chat_integration":"Integraciones de chat"}}},"retort":{"title":"añadir un emoji para reaccionar a este post"},"discourse_push_notifications":{"title":"Notificaciones Push","disable":"Desactivar notificaciones push","enable":"Activar notificaciones push","enable_note":"Tendrías que cambiar esta opción en todos los navegadores que utilices y esto desactivará las notificaciones de escritorio.","disable_note":"Tendrías que cambiar esta opción en todos los navegadores que utilices."},"user_notes":{"title":"Notas del usuario","attach":"Añadir nota de usuario","remove":"Quitar nota de usuario","delete_confirm":"¿Estás seguro de que deseas eliminar esta nota de usuario?","show_post":"Mostrar publicación"},"docker":{"upgrade":"Tu versión de Discourse está desactualizada.","perform_upgrade":"Haz clic aquí para actualizar.","link_to_upgrade":"Actualiza desde aquí."},"chat_integration":{"menu_title":"Integraciones de chat","settings":"Ajustes","no_providers":"Necesitas habilitar algún proveedor en los ajustes del plugin","channels_with_errors":"Algunos canales de este proveedores fallaron la última vez que se enviaron mensajes. Haz clic en los iconos de error para saber más.","channel_exception":"Un error desconocido se produjo la última vez que se mandó un mensaje a este canal.","choose_group":"(escoge un grupo)","all_categories":"(todas las categorías)","all_tags":"(todas las etiquetas)","create_rule":"Crear regla","create_channel":"Crear canal","delete_channel":"Eliminar","test_channel":"Probar","edit_channel":"Editar","channel_delete_confirm":"¿Estás seguro de que quieres eliminar este canal? Todas las reglas relacionadas se eliminarán.","test_modal":{"title":"Enviar un mensaje de prueba","topic":"Tema","send":"Enviar mensaje de prueba","close":"Cerrar","error":"Un error desconocido se produjo al enviar el mensaje. Revisa los registros de la página para más información.","success":"Mensaje enviado correctamente."},"type":{"normal":"Normal","group_message":"Mensaje grupal","group_mention":"Mención grupal"},"filter":{"mute":"Silenciar","follow":"Solo la primera publicación","watch":"Todas las publicaciones y respuestas"},"rule_table":{"filter":"Filtrar","category":"Categoría","tags":"Etiquetas","edit_rule":"Editar","delete_rule":"Eliminar"},"edit_channel_modal":{"title":"Editar canal","save":"Guardar canal","cancel":"Cancelar","provider":"Proveedor","channel_validation":{"ok":"Válido","fail":"Formato inválido"}},"edit_rule_modal":{"title":"Editar regla","save":"Guardar regla","cancel":"Cancelar","provider":"Proveedor","type":"Tipo","channel":"Canal","filter":"Filtro","category":"Categoría","group":"Grupo","tags":"Etiquetas","instructions":{"type":"Cambia el tipo para que aparezcan notificaciones de menciones o mensajes de grupo","filter":"Nivel de notificación. Silenciado anula otras reglas activadas.","category":"Esta regla solo se aplicará a los temas en la categoría especificada.","group":"Esta regla se aplicará a las publicaciones que hacen referencia a este grupo.","tags":"Si están especificadas, esta regla solo se aplicará a temas que tengan al menos una de estas etiquetas:"}},"provider":{"slack":{"title":"Slack","param":{"identifier":{"title":"Canal","help":"ejemplo: #canal, @usuario."}},"errors":{"action_prohibited":"Este bot no tiene permiso para publicar en ese canal.","channel_not_found":"El canal especificado no existe en Slack"}},"telegram":{"title":"Telegram","param":{"name":{"title":"Nombre","help":"Un nombre para describir el canal. No se usa en la conexión con Telegram."},"chat_id":{"title":"ID del chat","help":"Un número que el bot te ha dado o un identificador de canal de difusión en forma de @nombredelcanal"}},"errors":{"channel_not_found":"El canal especificado no existe en Telegram","forbidden":"El bot no tiene permiso para publicar en este canal"}},"discord":{"title":"Discord","param":{"name":{"title":"Nombre","help":"Un nombre para describir el canal. No se usará para la conexión con Discord."},"webhook_url":{"title":"URL del webhook","help":"La URL del webhook creado en los ajustes de tu servidor de Discord"}}},"mattermost":{"title":"Mattermost","param":{"identifier":{"title":"Canal","help":"ejemplo: #canal, @usuario."}},"errors":{"channel_not_found":"El canal especificado no existe en Mattermost"}},"matrix":{"title":"Matrix","param":{"name":{"title":"Nombre","help":"Un nombre para describir el canal. No se usa para la conexión con Matrix."},"room_id":{"title":"ID de la sala","help":"El «identificador privado» de la sala. Debería ser algo como !abcdefg:matrix.org"}},"errors":{"unknown_token":"Token de acceso inválido","unknown_room":"La ID de la sala no es válida"}},"zulip":{"title":"Zulip","param":{"stream":{"title":"Stream","help":"El nombre del stream de Zulip al que el mensaje debería ser enviado. Ejemplo: «general»"},"subject":{"title":"Asunto","help":"El asunto que el mensaje enviado por el bot debería tener"}},"errors":{"does_not_exist":"El stream especificado no existe en Zulip"}},"rocketchat":{"title":"Rocket.Chat","param":{"identifier":{"title":"Canal","help":"ejemplo: #canal, @usuario."}},"errors":{"invalid_channel":"Ese canal no existe en Rocket Chat"}},"gitter":{"title":"Gitter","param":{"name":{"title":"Nombre","help":"El nombre de la sala de Gitter. ejemplo: gitterHQ/services."},"webhook_url":{"title":"URL del webhook","help":"La URL dada cuando creaste una nueva integración en una sala de Gitter."}}},"flowdock":{"title":"Flowdock","param":{"flow_token":{"title":"Token de Flow","help":"El token de Flow provisto luego de crear una fuente para un flow al que quieras enviar mensajes."}}}}}}},"en":{"js":{"x_more":{"one":"%{count} More","other":"%{count} More"},"groups":{"members":{"make_primary":"Make Primary","make_all_primary_description":"Make this the primary group for all selected users","remove_all_primary":"Remove as Primary","remove_all_primary_description":"Remove this group as primary","primary":"Primary"}},"categories":{"topic_stat":{"one":"%{number} / %{unit}","other":"%{number} / %{unit}"},"topic_stat_all_time":{"one":"%{number} total","other":"%{number} total"}},"user":{"read_help":"Recently read topics","avatar":{"name_and_description":"%{name} - %{description}"}},"summary":{"description":{"one":"There is \u003cb\u003e%{count}\u003c/b\u003e reply.","other":"There are \u003cb\u003e%{count}\u003c/b\u003e replies."}},"login":{"header_title":"Welcome Back"},"composer":{"error":{"title_too_short":{"one":"Title must be at least %{count} character","other":"Title must be at least %{count} characters"},"title_too_long":{"one":"Title can't be more than %{count} character","other":"Title can't be more than %{count} characters"}}},"topic":{"topic_status_update":{"min_duration":"Duration must be greater than 0","max_duration":"Duration must be less than 2 years"},"auto_close_momentarily":{"one":"The last post in the topic is already %{count} hour old, so the topic will be closed momentarily.","other":"The last post in the topic is already %{count} hours old, so the topic will be closed momentarily."},"progress":{"jump_prompt_of":{"one":"of %{count} post","other":"of %{count} posts"}},"invite_reply":{"to_forum":"We'll send a brief email allowing your friend to immediately join by clicking a link.","success_email":"We mailed out an invitation to \u003cb\u003e%{invitee}\u003c/b\u003e. We'll notify you when the invitation is redeemed. Check the invitations tab on your user page to keep track of your invites."}},"post":{"wiki_last_edited_on":"wiki last edited on %{dateTime}","last_edited_on":"post last edited on %{dateTime}","view_all_posts":"View all posts","controls":{"delete_topic_confirm_modal":{"one":"This topic currently has over %{count} view and may be a popular search destination. Are you sure you want to delete this topic entirely, instead of editing it to improve it?","other":"This topic currently has over %{count} views and may be a popular search destination. Are you sure you want to delete this topic entirely, instead of editing it to improve it?"}}},"category":{"notifications":{"watching":{"description":"You will automatically watch all topics in this category. You will be notified of every new post in every topic, and a count of new replies will be shown."},"tracking":{"description":"You will automatically track all topics in this category. You will be notified if someone mentions your @name or replies to you, and a count of new replies will be shown."},"muted":{"description":"You will never be notified of anything about new topics in this category, and they will not appear in latest."}}},"tagging":{"tag_list_joiner":", "},"discourse_local_dates":{"create":{"form":{"format_description":"Format used to display the date to the user. Use Z to show the offset and zz for the timezone name."}}},"admin":{"procourse-installer":{"titles":{"install":"Install","installed":"Installed Plugins"},"remove":{"modal":"Are you sure you want to remove \u003cstrong\u003e%{plugin_name}\u003c/strong\u003e?"}}},"retort":{"reactions":{"one_person":"%{first} reacted with :%{emoji}:","two_people":"%{first} and %{second} reacted with :%{emoji}:","many_people":"%{first}, %{second}, and %{count} others reacted with :%{emoji}:"}},"discourse_push_notifications":{"note":"Prefer push notifications on desktop"},"user_notes":{"show":"User Notes (%{count})"},"chat_integration":{"group_mention_template":"Mentions of: @%{name}","group_message_template":"Messages to: @%{name}","filter":{"thread":"All posts with threaded replies"},"provider":{"groupme":{"title":"GroupMe","param":{"groupme_instance_name":{"title":"GroupMe Instance Name","help":"name of the Groupme instance as listed in Site Settings. use 'all' to send to all  instances"}},"errors":{"not_found":"The path you attempted to post your message to was not found. Check the Bot ID in Site Settings.","instance_names_issue":"instance names incorrectly formatted or not provided"}},"teams":{"title":"Microsoft Teams","param":{"name":{"title":"Name","help":"A Teams channel name e.g. discourse"},"webhook_url":{"title":"Webhook URL","help":"The URL provided when you create a new incomming webhook"}},"errors":{"invalid_channel":"That channel does not exist on Microsoft Teams"}},"webex":{"title":"Webex Teams","param":{"name":{"title":"Name","help":"A Webex space name e.g. discourse"},"webhook_url":{"title":"Webhook URL","help":"The URL provided when you create a new incomming webhook"}},"errors":{"invalid_channel":"That channel does not exist on Webex"}},"google":{"title":"Google Chat","param":{"name":{"title":"Name","help":"A name for the channel (only shown in the Discourse admin interface)"},"webhook_url":{"title":"Webhook URL","help":"The URL provided when you create a new webhook"}}}}},"procourse_installer":{"title":"ProCourse Installer","plugin_url":"Enter the url for the repo of the plugin. Ex: https://github.com/procourse/procourse-memberships","install":"Install Plugin","installed":"Plugin successfully installed!","uninstall":"Uninstall","uninstalled":"Plugin successfully uninstalled!"},"fingerprint":{"title":"Fingerprint","latest_matches":"Latest Matched Fingerprints","latest_matches_instructions":"\u003cp\u003e\u003csmall\u003eUser devices will be fingerprinted once per session using {{algorithm}}. For more information about fingerprinting algorithms and provide feedback, please go to \u003ca href=\"https://meta.discourse.org/t/discourse-fingerprint-browser-fingerprinting-plugin/114890\"\u003ethis topic on Discourse Meta\u003c/a\u003e.\u003c/small\u003e\u003c/p\u003e\n","hide_common":"Hide common","flagged":"Hidden or Silenced Fingerprints","flagged_instructions":"\u003cp\u003e\nA \u003ci\u003ehidden fingerprint\u003c/i\u003e will not be shown in the \u003ci\u003eLatest Matched Fingerprints\u003c/i\u003e section.\nA \u003ci\u003esilenced fingerprint\u003c/i\u003e will cause all users with the given fingerprint to be silenced next time they visit.\n\u003c/p\u003e\n\n\u003cp\u003e\nSilencing a fingerprint should be used only as a last resort as it is not guaranteed that it always matches the same user.\nFor example, users with similar configuration and hardware (devices of same make and model) have a high chance of having matching fingerprints.\n\u003c/p\u003e\n","flagged_not_found":"No fingerprints were hidden or silenced.","matches_for":"Matches for","matches_found":{"one":"{{count}} user has matching fingerprints.","other":"{{count}} users have matching fingerprints."},"matches_not_found":"No similar signature were found.","none":"This user has no recorded fingerprints.","common_device":"This fingerprints looks like it is coming from a common device. The report may not be relevant.","hide":"Hide","unhide":"Unhide","silence":"Silence","unsilence":"Unsilence","ignore":"Ignore","unignore":"Unignore","details":"Details","results":{"matching_user":"Matching User","hash":"Algorithm and Hash","first_seen":"First Seen","last_seen":"Last Seen","matches":"Matches"}}}}};
I18n.locale = 'es';
I18n.pluralizationRules.es = MessageFormat.locale.es;
//! moment.js
//! version : 2.29.1
//! authors : Tim Wood, Iskren Chernev, Moment.js contributors
//! license : MIT
//! momentjs.com

;(function (global, factory) {
    typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
    typeof define === 'function' && define.amd ? define(factory) :
    global.moment = factory()
}(this, (function () { 'use strict';

    var hookCallback;

    function hooks() {
        return hookCallback.apply(null, arguments);
    }

    // This is done to register the method called with moment()
    // without creating circular dependencies.
    function setHookCallback(callback) {
        hookCallback = callback;
    }

    function isArray(input) {
        return (
            input instanceof Array ||
            Object.prototype.toString.call(input) === '[object Array]'
        );
    }

    function isObject(input) {
        // IE8 will treat undefined and null as object if it wasn't for
        // input != null
        return (
            input != null &&
            Object.prototype.toString.call(input) === '[object Object]'
        );
    }

    function hasOwnProp(a, b) {
        return Object.prototype.hasOwnProperty.call(a, b);
    }

    function isObjectEmpty(obj) {
        if (Object.getOwnPropertyNames) {
            return Object.getOwnPropertyNames(obj).length === 0;
        } else {
            var k;
            for (k in obj) {
                if (hasOwnProp(obj, k)) {
                    return false;
                }
            }
            return true;
        }
    }

    function isUndefined(input) {
        return input === void 0;
    }

    function isNumber(input) {
        return (
            typeof input === 'number' ||
            Object.prototype.toString.call(input) === '[object Number]'
        );
    }

    function isDate(input) {
        return (
            input instanceof Date ||
            Object.prototype.toString.call(input) === '[object Date]'
        );
    }

    function map(arr, fn) {
        var res = [],
            i;
        for (i = 0; i < arr.length; ++i) {
            res.push(fn(arr[i], i));
        }
        return res;
    }

    function extend(a, b) {
        for (var i in b) {
            if (hasOwnProp(b, i)) {
                a[i] = b[i];
            }
        }

        if (hasOwnProp(b, 'toString')) {
            a.toString = b.toString;
        }

        if (hasOwnProp(b, 'valueOf')) {
            a.valueOf = b.valueOf;
        }

        return a;
    }

    function createUTC(input, format, locale, strict) {
        return createLocalOrUTC(input, format, locale, strict, true).utc();
    }

    function defaultParsingFlags() {
        // We need to deep clone this object.
        return {
            empty: false,
            unusedTokens: [],
            unusedInput: [],
            overflow: -2,
            charsLeftOver: 0,
            nullInput: false,
            invalidEra: null,
            invalidMonth: null,
            invalidFormat: false,
            userInvalidated: false,
            iso: false,
            parsedDateParts: [],
            era: null,
            meridiem: null,
            rfc2822: false,
            weekdayMismatch: false,
        };
    }

    function getParsingFlags(m) {
        if (m._pf == null) {
            m._pf = defaultParsingFlags();
        }
        return m._pf;
    }

    var some;
    if (Array.prototype.some) {
        some = Array.prototype.some;
    } else {
        some = function (fun) {
            var t = Object(this),
                len = t.length >>> 0,
                i;

            for (i = 0; i < len; i++) {
                if (i in t && fun.call(this, t[i], i, t)) {
                    return true;
                }
            }

            return false;
        };
    }

    function isValid(m) {
        if (m._isValid == null) {
            var flags = getParsingFlags(m),
                parsedParts = some.call(flags.parsedDateParts, function (i) {
                    return i != null;
                }),
                isNowValid =
                    !isNaN(m._d.getTime()) &&
                    flags.overflow < 0 &&
                    !flags.empty &&
                    !flags.invalidEra &&
                    !flags.invalidMonth &&
                    !flags.invalidWeekday &&
                    !flags.weekdayMismatch &&
                    !flags.nullInput &&
                    !flags.invalidFormat &&
                    !flags.userInvalidated &&
                    (!flags.meridiem || (flags.meridiem && parsedParts));

            if (m._strict) {
                isNowValid =
                    isNowValid &&
                    flags.charsLeftOver === 0 &&
                    flags.unusedTokens.length === 0 &&
                    flags.bigHour === undefined;
            }

            if (Object.isFrozen == null || !Object.isFrozen(m)) {
                m._isValid = isNowValid;
            } else {
                return isNowValid;
            }
        }
        return m._isValid;
    }

    function createInvalid(flags) {
        var m = createUTC(NaN);
        if (flags != null) {
            extend(getParsingFlags(m), flags);
        } else {
            getParsingFlags(m).userInvalidated = true;
        }

        return m;
    }

    // Plugins that add properties should also add the key here (null value),
    // so we can properly clone ourselves.
    var momentProperties = (hooks.momentProperties = []),
        updateInProgress = false;

    function copyConfig(to, from) {
        var i, prop, val;

        if (!isUndefined(from._isAMomentObject)) {
            to._isAMomentObject = from._isAMomentObject;
        }
        if (!isUndefined(from._i)) {
            to._i = from._i;
        }
        if (!isUndefined(from._f)) {
            to._f = from._f;
        }
        if (!isUndefined(from._l)) {
            to._l = from._l;
        }
        if (!isUndefined(from._strict)) {
            to._strict = from._strict;
        }
        if (!isUndefined(from._tzm)) {
            to._tzm = from._tzm;
        }
        if (!isUndefined(from._isUTC)) {
            to._isUTC = from._isUTC;
        }
        if (!isUndefined(from._offset)) {
            to._offset = from._offset;
        }
        if (!isUndefined(from._pf)) {
            to._pf = getParsingFlags(from);
        }
        if (!isUndefined(from._locale)) {
            to._locale = from._locale;
        }

        if (momentProperties.length > 0) {
            for (i = 0; i < momentProperties.length; i++) {
                prop = momentProperties[i];
                val = from[prop];
                if (!isUndefined(val)) {
                    to[prop] = val;
                }
            }
        }

        return to;
    }

    // Moment prototype object
    function Moment(config) {
        copyConfig(this, config);
        this._d = new Date(config._d != null ? config._d.getTime() : NaN);
        if (!this.isValid()) {
            this._d = new Date(NaN);
        }
        // Prevent infinite loop in case updateOffset creates new moment
        // objects.
        if (updateInProgress === false) {
            updateInProgress = true;
            hooks.updateOffset(this);
            updateInProgress = false;
        }
    }

    function isMoment(obj) {
        return (
            obj instanceof Moment || (obj != null && obj._isAMomentObject != null)
        );
    }

    function warn(msg) {
        if (
            hooks.suppressDeprecationWarnings === false &&
            typeof console !== 'undefined' &&
            console.warn
        ) {
            console.warn('Deprecation warning: ' + msg);
        }
    }

    function deprecate(msg, fn) {
        var firstTime = true;

        return extend(function () {
            if (hooks.deprecationHandler != null) {
                hooks.deprecationHandler(null, msg);
            }
            if (firstTime) {
                var args = [],
                    arg,
                    i,
                    key;
                for (i = 0; i < arguments.length; i++) {
                    arg = '';
                    if (typeof arguments[i] === 'object') {
                        arg += '\n[' + i + '] ';
                        for (key in arguments[0]) {
                            if (hasOwnProp(arguments[0], key)) {
                                arg += key + ': ' + arguments[0][key] + ', ';
                            }
                        }
                        arg = arg.slice(0, -2); // Remove trailing comma and space
                    } else {
                        arg = arguments[i];
                    }
                    args.push(arg);
                }
                warn(
                    msg +
                        '\nArguments: ' +
                        Array.prototype.slice.call(args).join('') +
                        '\n' +
                        new Error().stack
                );
                firstTime = false;
            }
            return fn.apply(this, arguments);
        }, fn);
    }

    var deprecations = {};

    function deprecateSimple(name, msg) {
        if (hooks.deprecationHandler != null) {
            hooks.deprecationHandler(name, msg);
        }
        if (!deprecations[name]) {
            warn(msg);
            deprecations[name] = true;
        }
    }

    hooks.suppressDeprecationWarnings = false;
    hooks.deprecationHandler = null;

    function isFunction(input) {
        return (
            (typeof Function !== 'undefined' && input instanceof Function) ||
            Object.prototype.toString.call(input) === '[object Function]'
        );
    }

    function set(config) {
        var prop, i;
        for (i in config) {
            if (hasOwnProp(config, i)) {
                prop = config[i];
                if (isFunction(prop)) {
                    this[i] = prop;
                } else {
                    this['_' + i] = prop;
                }
            }
        }
        this._config = config;
        // Lenient ordinal parsing accepts just a number in addition to
        // number + (possibly) stuff coming from _dayOfMonthOrdinalParse.
        // TODO: Remove "ordinalParse" fallback in next major release.
        this._dayOfMonthOrdinalParseLenient = new RegExp(
            (this._dayOfMonthOrdinalParse.source || this._ordinalParse.source) +
                '|' +
                /\d{1,2}/.source
        );
    }

    function mergeConfigs(parentConfig, childConfig) {
        var res = extend({}, parentConfig),
            prop;
        for (prop in childConfig) {
            if (hasOwnProp(childConfig, prop)) {
                if (isObject(parentConfig[prop]) && isObject(childConfig[prop])) {
                    res[prop] = {};
                    extend(res[prop], parentConfig[prop]);
                    extend(res[prop], childConfig[prop]);
                } else if (childConfig[prop] != null) {
                    res[prop] = childConfig[prop];
                } else {
                    delete res[prop];
                }
            }
        }
        for (prop in parentConfig) {
            if (
                hasOwnProp(parentConfig, prop) &&
                !hasOwnProp(childConfig, prop) &&
                isObject(parentConfig[prop])
            ) {
                // make sure changes to properties don't modify parent config
                res[prop] = extend({}, res[prop]);
            }
        }
        return res;
    }

    function Locale(config) {
        if (config != null) {
            this.set(config);
        }
    }

    var keys;

    if (Object.keys) {
        keys = Object.keys;
    } else {
        keys = function (obj) {
            var i,
                res = [];
            for (i in obj) {
                if (hasOwnProp(obj, i)) {
                    res.push(i);
                }
            }
            return res;
        };
    }

    var defaultCalendar = {
        sameDay: '[Today at] LT',
        nextDay: '[Tomorrow at] LT',
        nextWeek: 'dddd [at] LT',
        lastDay: '[Yesterday at] LT',
        lastWeek: '[Last] dddd [at] LT',
        sameElse: 'L',
    };

    function calendar(key, mom, now) {
        var output = this._calendar[key] || this._calendar['sameElse'];
        return isFunction(output) ? output.call(mom, now) : output;
    }

    function zeroFill(number, targetLength, forceSign) {
        var absNumber = '' + Math.abs(number),
            zerosToFill = targetLength - absNumber.length,
            sign = number >= 0;
        return (
            (sign ? (forceSign ? '+' : '') : '-') +
            Math.pow(10, Math.max(0, zerosToFill)).toString().substr(1) +
            absNumber
        );
    }

    var formattingTokens = /(\[[^\[]*\])|(\\)?([Hh]mm(ss)?|Mo|MM?M?M?|Do|DDDo|DD?D?D?|ddd?d?|do?|w[o|w]?|W[o|W]?|Qo?|N{1,5}|YYYYYY|YYYYY|YYYY|YY|y{2,4}|yo?|gg(ggg?)?|GG(GGG?)?|e|E|a|A|hh?|HH?|kk?|mm?|ss?|S{1,9}|x|X|zz?|ZZ?|.)/g,
        localFormattingTokens = /(\[[^\[]*\])|(\\)?(LTS|LT|LL?L?L?|l{1,4})/g,
        formatFunctions = {},
        formatTokenFunctions = {};

    // token:    'M'
    // padded:   ['MM', 2]
    // ordinal:  'Mo'
    // callback: function () { this.month() + 1 }
    function addFormatToken(token, padded, ordinal, callback) {
        var func = callback;
        if (typeof callback === 'string') {
            func = function () {
                return this[callback]();
            };
        }
        if (token) {
            formatTokenFunctions[token] = func;
        }
        if (padded) {
            formatTokenFunctions[padded[0]] = function () {
                return zeroFill(func.apply(this, arguments), padded[1], padded[2]);
            };
        }
        if (ordinal) {
            formatTokenFunctions[ordinal] = function () {
                return this.localeData().ordinal(
                    func.apply(this, arguments),
                    token
                );
            };
        }
    }

    function removeFormattingTokens(input) {
        if (input.match(/\[[\s\S]/)) {
            return input.replace(/^\[|\]$/g, '');
        }
        return input.replace(/\\/g, '');
    }

    function makeFormatFunction(format) {
        var array = format.match(formattingTokens),
            i,
            length;

        for (i = 0, length = array.length; i < length; i++) {
            if (formatTokenFunctions[array[i]]) {
                array[i] = formatTokenFunctions[array[i]];
            } else {
                array[i] = removeFormattingTokens(array[i]);
            }
        }

        return function (mom) {
            var output = '',
                i;
            for (i = 0; i < length; i++) {
                output += isFunction(array[i])
                    ? array[i].call(mom, format)
                    : array[i];
            }
            return output;
        };
    }

    // format date using native date object
    function formatMoment(m, format) {
        if (!m.isValid()) {
            return m.localeData().invalidDate();
        }

        format = expandFormat(format, m.localeData());
        formatFunctions[format] =
            formatFunctions[format] || makeFormatFunction(format);

        return formatFunctions[format](m);
    }

    function expandFormat(format, locale) {
        var i = 5;

        function replaceLongDateFormatTokens(input) {
            return locale.longDateFormat(input) || input;
        }

        localFormattingTokens.lastIndex = 0;
        while (i >= 0 && localFormattingTokens.test(format)) {
            format = format.replace(
                localFormattingTokens,
                replaceLongDateFormatTokens
            );
            localFormattingTokens.lastIndex = 0;
            i -= 1;
        }

        return format;
    }

    var defaultLongDateFormat = {
        LTS: 'h:mm:ss A',
        LT: 'h:mm A',
        L: 'MM/DD/YYYY',
        LL: 'MMMM D, YYYY',
        LLL: 'MMMM D, YYYY h:mm A',
        LLLL: 'dddd, MMMM D, YYYY h:mm A',
    };

    function longDateFormat(key) {
        var format = this._longDateFormat[key],
            formatUpper = this._longDateFormat[key.toUpperCase()];

        if (format || !formatUpper) {
            return format;
        }

        this._longDateFormat[key] = formatUpper
            .match(formattingTokens)
            .map(function (tok) {
                if (
                    tok === 'MMMM' ||
                    tok === 'MM' ||
                    tok === 'DD' ||
                    tok === 'dddd'
                ) {
                    return tok.slice(1);
                }
                return tok;
            })
            .join('');

        return this._longDateFormat[key];
    }

    var defaultInvalidDate = 'Invalid date';

    function invalidDate() {
        return this._invalidDate;
    }

    var defaultOrdinal = '%d',
        defaultDayOfMonthOrdinalParse = /\d{1,2}/;

    function ordinal(number) {
        return this._ordinal.replace('%d', number);
    }

    var defaultRelativeTime = {
        future: 'in %s',
        past: '%s ago',
        s: 'a few seconds',
        ss: '%d seconds',
        m: 'a minute',
        mm: '%d minutes',
        h: 'an hour',
        hh: '%d hours',
        d: 'a day',
        dd: '%d days',
        w: 'a week',
        ww: '%d weeks',
        M: 'a month',
        MM: '%d months',
        y: 'a year',
        yy: '%d years',
    };

    function relativeTime(number, withoutSuffix, string, isFuture) {
        var output = this._relativeTime[string];
        return isFunction(output)
            ? output(number, withoutSuffix, string, isFuture)
            : output.replace(/%d/i, number);
    }

    function pastFuture(diff, output) {
        var format = this._relativeTime[diff > 0 ? 'future' : 'past'];
        return isFunction(format) ? format(output) : format.replace(/%s/i, output);
    }

    var aliases = {};

    function addUnitAlias(unit, shorthand) {
        var lowerCase = unit.toLowerCase();
        aliases[lowerCase] = aliases[lowerCase + 's'] = aliases[shorthand] = unit;
    }

    function normalizeUnits(units) {
        return typeof units === 'string'
            ? aliases[units] || aliases[units.toLowerCase()]
            : undefined;
    }

    function normalizeObjectUnits(inputObject) {
        var normalizedInput = {},
            normalizedProp,
            prop;

        for (prop in inputObject) {
            if (hasOwnProp(inputObject, prop)) {
                normalizedProp = normalizeUnits(prop);
                if (normalizedProp) {
                    normalizedInput[normalizedProp] = inputObject[prop];
                }
            }
        }

        return normalizedInput;
    }

    var priorities = {};

    function addUnitPriority(unit, priority) {
        priorities[unit] = priority;
    }

    function getPrioritizedUnits(unitsObj) {
        var units = [],
            u;
        for (u in unitsObj) {
            if (hasOwnProp(unitsObj, u)) {
                units.push({ unit: u, priority: priorities[u] });
            }
        }
        units.sort(function (a, b) {
            return a.priority - b.priority;
        });
        return units;
    }

    function isLeapYear(year) {
        return (year % 4 === 0 && year % 100 !== 0) || year % 400 === 0;
    }

    function absFloor(number) {
        if (number < 0) {
            // -0 -> 0
            return Math.ceil(number) || 0;
        } else {
            return Math.floor(number);
        }
    }

    function toInt(argumentForCoercion) {
        var coercedNumber = +argumentForCoercion,
            value = 0;

        if (coercedNumber !== 0 && isFinite(coercedNumber)) {
            value = absFloor(coercedNumber);
        }

        return value;
    }

    function makeGetSet(unit, keepTime) {
        return function (value) {
            if (value != null) {
                set$1(this, unit, value);
                hooks.updateOffset(this, keepTime);
                return this;
            } else {
                return get(this, unit);
            }
        };
    }

    function get(mom, unit) {
        return mom.isValid()
            ? mom._d['get' + (mom._isUTC ? 'UTC' : '') + unit]()
            : NaN;
    }

    function set$1(mom, unit, value) {
        if (mom.isValid() && !isNaN(value)) {
            if (
                unit === 'FullYear' &&
                isLeapYear(mom.year()) &&
                mom.month() === 1 &&
                mom.date() === 29
            ) {
                value = toInt(value);
                mom._d['set' + (mom._isUTC ? 'UTC' : '') + unit](
                    value,
                    mom.month(),
                    daysInMonth(value, mom.month())
                );
            } else {
                mom._d['set' + (mom._isUTC ? 'UTC' : '') + unit](value);
            }
        }
    }

    // MOMENTS

    function stringGet(units) {
        units = normalizeUnits(units);
        if (isFunction(this[units])) {
            return this[units]();
        }
        return this;
    }

    function stringSet(units, value) {
        if (typeof units === 'object') {
            units = normalizeObjectUnits(units);
            var prioritized = getPrioritizedUnits(units),
                i;
            for (i = 0; i < prioritized.length; i++) {
                this[prioritized[i].unit](units[prioritized[i].unit]);
            }
        } else {
            units = normalizeUnits(units);
            if (isFunction(this[units])) {
                return this[units](value);
            }
        }
        return this;
    }

    var match1 = /\d/, //       0 - 9
        match2 = /\d\d/, //      00 - 99
        match3 = /\d{3}/, //     000 - 999
        match4 = /\d{4}/, //    0000 - 9999
        match6 = /[+-]?\d{6}/, // -999999 - 999999
        match1to2 = /\d\d?/, //       0 - 99
        match3to4 = /\d\d\d\d?/, //     999 - 9999
        match5to6 = /\d\d\d\d\d\d?/, //   99999 - 999999
        match1to3 = /\d{1,3}/, //       0 - 999
        match1to4 = /\d{1,4}/, //       0 - 9999
        match1to6 = /[+-]?\d{1,6}/, // -999999 - 999999
        matchUnsigned = /\d+/, //       0 - inf
        matchSigned = /[+-]?\d+/, //    -inf - inf
        matchOffset = /Z|[+-]\d\d:?\d\d/gi, // +00:00 -00:00 +0000 -0000 or Z
        matchShortOffset = /Z|[+-]\d\d(?::?\d\d)?/gi, // +00 -00 +00:00 -00:00 +0000 -0000 or Z
        matchTimestamp = /[+-]?\d+(\.\d{1,3})?/, // 123456789 123456789.123
        // any word (or two) characters or numbers including two/three word month in arabic.
        // includes scottish gaelic two word and hyphenated months
        matchWord = /[0-9]{0,256}['a-z\u00A0-\u05FF\u0700-\uD7FF\uF900-\uFDCF\uFDF0-\uFF07\uFF10-\uFFEF]{1,256}|[\u0600-\u06FF\/]{1,256}(\s*?[\u0600-\u06FF]{1,256}){1,2}/i,
        regexes;

    regexes = {};

    function addRegexToken(token, regex, strictRegex) {
        regexes[token] = isFunction(regex)
            ? regex
            : function (isStrict, localeData) {
                  return isStrict && strictRegex ? strictRegex : regex;
              };
    }

    function getParseRegexForToken(token, config) {
        if (!hasOwnProp(regexes, token)) {
            return new RegExp(unescapeFormat(token));
        }

        return regexes[token](config._strict, config._locale);
    }

    // Code from http://stackoverflow.com/questions/3561493/is-there-a-regexp-escape-function-in-javascript
    function unescapeFormat(s) {
        return regexEscape(
            s
                .replace('\\', '')
                .replace(/\\(\[)|\\(\])|\[([^\]\[]*)\]|\\(.)/g, function (
                    matched,
                    p1,
                    p2,
                    p3,
                    p4
                ) {
                    return p1 || p2 || p3 || p4;
                })
        );
    }

    function regexEscape(s) {
        return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    }

    var tokens = {};

    function addParseToken(token, callback) {
        var i,
            func = callback;
        if (typeof token === 'string') {
            token = [token];
        }
        if (isNumber(callback)) {
            func = function (input, array) {
                array[callback] = toInt(input);
            };
        }
        for (i = 0; i < token.length; i++) {
            tokens[token[i]] = func;
        }
    }

    function addWeekParseToken(token, callback) {
        addParseToken(token, function (input, array, config, token) {
            config._w = config._w || {};
            callback(input, config._w, config, token);
        });
    }

    function addTimeToArrayFromToken(token, input, config) {
        if (input != null && hasOwnProp(tokens, token)) {
            tokens[token](input, config._a, config, token);
        }
    }

    var YEAR = 0,
        MONTH = 1,
        DATE = 2,
        HOUR = 3,
        MINUTE = 4,
        SECOND = 5,
        MILLISECOND = 6,
        WEEK = 7,
        WEEKDAY = 8;

    function mod(n, x) {
        return ((n % x) + x) % x;
    }

    var indexOf;

    if (Array.prototype.indexOf) {
        indexOf = Array.prototype.indexOf;
    } else {
        indexOf = function (o) {
            // I know
            var i;
            for (i = 0; i < this.length; ++i) {
                if (this[i] === o) {
                    return i;
                }
            }
            return -1;
        };
    }

    function daysInMonth(year, month) {
        if (isNaN(year) || isNaN(month)) {
            return NaN;
        }
        var modMonth = mod(month, 12);
        year += (month - modMonth) / 12;
        return modMonth === 1
            ? isLeapYear(year)
                ? 29
                : 28
            : 31 - ((modMonth % 7) % 2);
    }

    // FORMATTING

    addFormatToken('M', ['MM', 2], 'Mo', function () {
        return this.month() + 1;
    });

    addFormatToken('MMM', 0, 0, function (format) {
        return this.localeData().monthsShort(this, format);
    });

    addFormatToken('MMMM', 0, 0, function (format) {
        return this.localeData().months(this, format);
    });

    // ALIASES

    addUnitAlias('month', 'M');

    // PRIORITY

    addUnitPriority('month', 8);

    // PARSING

    addRegexToken('M', match1to2);
    addRegexToken('MM', match1to2, match2);
    addRegexToken('MMM', function (isStrict, locale) {
        return locale.monthsShortRegex(isStrict);
    });
    addRegexToken('MMMM', function (isStrict, locale) {
        return locale.monthsRegex(isStrict);
    });

    addParseToken(['M', 'MM'], function (input, array) {
        array[MONTH] = toInt(input) - 1;
    });

    addParseToken(['MMM', 'MMMM'], function (input, array, config, token) {
        var month = config._locale.monthsParse(input, token, config._strict);
        // if we didn't find a month name, mark the date as invalid.
        if (month != null) {
            array[MONTH] = month;
        } else {
            getParsingFlags(config).invalidMonth = input;
        }
    });

    // LOCALES

    var defaultLocaleMonths = 'January_February_March_April_May_June_July_August_September_October_November_December'.split(
            '_'
        ),
        defaultLocaleMonthsShort = 'Jan_Feb_Mar_Apr_May_Jun_Jul_Aug_Sep_Oct_Nov_Dec'.split(
            '_'
        ),
        MONTHS_IN_FORMAT = /D[oD]?(\[[^\[\]]*\]|\s)+MMMM?/,
        defaultMonthsShortRegex = matchWord,
        defaultMonthsRegex = matchWord;

    function localeMonths(m, format) {
        if (!m) {
            return isArray(this._months)
                ? this._months
                : this._months['standalone'];
        }
        return isArray(this._months)
            ? this._months[m.month()]
            : this._months[
                  (this._months.isFormat || MONTHS_IN_FORMAT).test(format)
                      ? 'format'
                      : 'standalone'
              ][m.month()];
    }

    function localeMonthsShort(m, format) {
        if (!m) {
            return isArray(this._monthsShort)
                ? this._monthsShort
                : this._monthsShort['standalone'];
        }
        return isArray(this._monthsShort)
            ? this._monthsShort[m.month()]
            : this._monthsShort[
                  MONTHS_IN_FORMAT.test(format) ? 'format' : 'standalone'
              ][m.month()];
    }

    function handleStrictParse(monthName, format, strict) {
        var i,
            ii,
            mom,
            llc = monthName.toLocaleLowerCase();
        if (!this._monthsParse) {
            // this is not used
            this._monthsParse = [];
            this._longMonthsParse = [];
            this._shortMonthsParse = [];
            for (i = 0; i < 12; ++i) {
                mom = createUTC([2000, i]);
                this._shortMonthsParse[i] = this.monthsShort(
                    mom,
                    ''
                ).toLocaleLowerCase();
                this._longMonthsParse[i] = this.months(mom, '').toLocaleLowerCase();
            }
        }

        if (strict) {
            if (format === 'MMM') {
                ii = indexOf.call(this._shortMonthsParse, llc);
                return ii !== -1 ? ii : null;
            } else {
                ii = indexOf.call(this._longMonthsParse, llc);
                return ii !== -1 ? ii : null;
            }
        } else {
            if (format === 'MMM') {
                ii = indexOf.call(this._shortMonthsParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._longMonthsParse, llc);
                return ii !== -1 ? ii : null;
            } else {
                ii = indexOf.call(this._longMonthsParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._shortMonthsParse, llc);
                return ii !== -1 ? ii : null;
            }
        }
    }

    function localeMonthsParse(monthName, format, strict) {
        var i, mom, regex;

        if (this._monthsParseExact) {
            return handleStrictParse.call(this, monthName, format, strict);
        }

        if (!this._monthsParse) {
            this._monthsParse = [];
            this._longMonthsParse = [];
            this._shortMonthsParse = [];
        }

        // TODO: add sorting
        // Sorting makes sure if one month (or abbr) is a prefix of another
        // see sorting in computeMonthsParse
        for (i = 0; i < 12; i++) {
            // make the regex if we don't have it already
            mom = createUTC([2000, i]);
            if (strict && !this._longMonthsParse[i]) {
                this._longMonthsParse[i] = new RegExp(
                    '^' + this.months(mom, '').replace('.', '') + '$',
                    'i'
                );
                this._shortMonthsParse[i] = new RegExp(
                    '^' + this.monthsShort(mom, '').replace('.', '') + '$',
                    'i'
                );
            }
            if (!strict && !this._monthsParse[i]) {
                regex =
                    '^' + this.months(mom, '') + '|^' + this.monthsShort(mom, '');
                this._monthsParse[i] = new RegExp(regex.replace('.', ''), 'i');
            }
            // test the regex
            if (
                strict &&
                format === 'MMMM' &&
                this._longMonthsParse[i].test(monthName)
            ) {
                return i;
            } else if (
                strict &&
                format === 'MMM' &&
                this._shortMonthsParse[i].test(monthName)
            ) {
                return i;
            } else if (!strict && this._monthsParse[i].test(monthName)) {
                return i;
            }
        }
    }

    // MOMENTS

    function setMonth(mom, value) {
        var dayOfMonth;

        if (!mom.isValid()) {
            // No op
            return mom;
        }

        if (typeof value === 'string') {
            if (/^\d+$/.test(value)) {
                value = toInt(value);
            } else {
                value = mom.localeData().monthsParse(value);
                // TODO: Another silent failure?
                if (!isNumber(value)) {
                    return mom;
                }
            }
        }

        dayOfMonth = Math.min(mom.date(), daysInMonth(mom.year(), value));
        mom._d['set' + (mom._isUTC ? 'UTC' : '') + 'Month'](value, dayOfMonth);
        return mom;
    }

    function getSetMonth(value) {
        if (value != null) {
            setMonth(this, value);
            hooks.updateOffset(this, true);
            return this;
        } else {
            return get(this, 'Month');
        }
    }

    function getDaysInMonth() {
        return daysInMonth(this.year(), this.month());
    }

    function monthsShortRegex(isStrict) {
        if (this._monthsParseExact) {
            if (!hasOwnProp(this, '_monthsRegex')) {
                computeMonthsParse.call(this);
            }
            if (isStrict) {
                return this._monthsShortStrictRegex;
            } else {
                return this._monthsShortRegex;
            }
        } else {
            if (!hasOwnProp(this, '_monthsShortRegex')) {
                this._monthsShortRegex = defaultMonthsShortRegex;
            }
            return this._monthsShortStrictRegex && isStrict
                ? this._monthsShortStrictRegex
                : this._monthsShortRegex;
        }
    }

    function monthsRegex(isStrict) {
        if (this._monthsParseExact) {
            if (!hasOwnProp(this, '_monthsRegex')) {
                computeMonthsParse.call(this);
            }
            if (isStrict) {
                return this._monthsStrictRegex;
            } else {
                return this._monthsRegex;
            }
        } else {
            if (!hasOwnProp(this, '_monthsRegex')) {
                this._monthsRegex = defaultMonthsRegex;
            }
            return this._monthsStrictRegex && isStrict
                ? this._monthsStrictRegex
                : this._monthsRegex;
        }
    }

    function computeMonthsParse() {
        function cmpLenRev(a, b) {
            return b.length - a.length;
        }

        var shortPieces = [],
            longPieces = [],
            mixedPieces = [],
            i,
            mom;
        for (i = 0; i < 12; i++) {
            // make the regex if we don't have it already
            mom = createUTC([2000, i]);
            shortPieces.push(this.monthsShort(mom, ''));
            longPieces.push(this.months(mom, ''));
            mixedPieces.push(this.months(mom, ''));
            mixedPieces.push(this.monthsShort(mom, ''));
        }
        // Sorting makes sure if one month (or abbr) is a prefix of another it
        // will match the longer piece.
        shortPieces.sort(cmpLenRev);
        longPieces.sort(cmpLenRev);
        mixedPieces.sort(cmpLenRev);
        for (i = 0; i < 12; i++) {
            shortPieces[i] = regexEscape(shortPieces[i]);
            longPieces[i] = regexEscape(longPieces[i]);
        }
        for (i = 0; i < 24; i++) {
            mixedPieces[i] = regexEscape(mixedPieces[i]);
        }

        this._monthsRegex = new RegExp('^(' + mixedPieces.join('|') + ')', 'i');
        this._monthsShortRegex = this._monthsRegex;
        this._monthsStrictRegex = new RegExp(
            '^(' + longPieces.join('|') + ')',
            'i'
        );
        this._monthsShortStrictRegex = new RegExp(
            '^(' + shortPieces.join('|') + ')',
            'i'
        );
    }

    // FORMATTING

    addFormatToken('Y', 0, 0, function () {
        var y = this.year();
        return y <= 9999 ? zeroFill(y, 4) : '+' + y;
    });

    addFormatToken(0, ['YY', 2], 0, function () {
        return this.year() % 100;
    });

    addFormatToken(0, ['YYYY', 4], 0, 'year');
    addFormatToken(0, ['YYYYY', 5], 0, 'year');
    addFormatToken(0, ['YYYYYY', 6, true], 0, 'year');

    // ALIASES

    addUnitAlias('year', 'y');

    // PRIORITIES

    addUnitPriority('year', 1);

    // PARSING

    addRegexToken('Y', matchSigned);
    addRegexToken('YY', match1to2, match2);
    addRegexToken('YYYY', match1to4, match4);
    addRegexToken('YYYYY', match1to6, match6);
    addRegexToken('YYYYYY', match1to6, match6);

    addParseToken(['YYYYY', 'YYYYYY'], YEAR);
    addParseToken('YYYY', function (input, array) {
        array[YEAR] =
            input.length === 2 ? hooks.parseTwoDigitYear(input) : toInt(input);
    });
    addParseToken('YY', function (input, array) {
        array[YEAR] = hooks.parseTwoDigitYear(input);
    });
    addParseToken('Y', function (input, array) {
        array[YEAR] = parseInt(input, 10);
    });

    // HELPERS

    function daysInYear(year) {
        return isLeapYear(year) ? 366 : 365;
    }

    // HOOKS

    hooks.parseTwoDigitYear = function (input) {
        return toInt(input) + (toInt(input) > 68 ? 1900 : 2000);
    };

    // MOMENTS

    var getSetYear = makeGetSet('FullYear', true);

    function getIsLeapYear() {
        return isLeapYear(this.year());
    }

    function createDate(y, m, d, h, M, s, ms) {
        // can't just apply() to create a date:
        // https://stackoverflow.com/q/181348
        var date;
        // the date constructor remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
            // preserve leap years using a full 400 year cycle, then reset
            date = new Date(y + 400, m, d, h, M, s, ms);
            if (isFinite(date.getFullYear())) {
                date.setFullYear(y);
            }
        } else {
            date = new Date(y, m, d, h, M, s, ms);
        }

        return date;
    }

    function createUTCDate(y) {
        var date, args;
        // the Date.UTC function remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
            args = Array.prototype.slice.call(arguments);
            // preserve leap years using a full 400 year cycle, then reset
            args[0] = y + 400;
            date = new Date(Date.UTC.apply(null, args));
            if (isFinite(date.getUTCFullYear())) {
                date.setUTCFullYear(y);
            }
        } else {
            date = new Date(Date.UTC.apply(null, arguments));
        }

        return date;
    }

    // start-of-first-week - start-of-year
    function firstWeekOffset(year, dow, doy) {
        var // first-week day -- which january is always in the first week (4 for iso, 1 for other)
            fwd = 7 + dow - doy,
            // first-week day local weekday -- which local weekday is fwd
            fwdlw = (7 + createUTCDate(year, 0, fwd).getUTCDay() - dow) % 7;

        return -fwdlw + fwd - 1;
    }

    // https://en.wikipedia.org/wiki/ISO_week_date#Calculating_a_date_given_the_year.2C_week_number_and_weekday
    function dayOfYearFromWeeks(year, week, weekday, dow, doy) {
        var localWeekday = (7 + weekday - dow) % 7,
            weekOffset = firstWeekOffset(year, dow, doy),
            dayOfYear = 1 + 7 * (week - 1) + localWeekday + weekOffset,
            resYear,
            resDayOfYear;

        if (dayOfYear <= 0) {
            resYear = year - 1;
            resDayOfYear = daysInYear(resYear) + dayOfYear;
        } else if (dayOfYear > daysInYear(year)) {
            resYear = year + 1;
            resDayOfYear = dayOfYear - daysInYear(year);
        } else {
            resYear = year;
            resDayOfYear = dayOfYear;
        }

        return {
            year: resYear,
            dayOfYear: resDayOfYear,
        };
    }

    function weekOfYear(mom, dow, doy) {
        var weekOffset = firstWeekOffset(mom.year(), dow, doy),
            week = Math.floor((mom.dayOfYear() - weekOffset - 1) / 7) + 1,
            resWeek,
            resYear;

        if (week < 1) {
            resYear = mom.year() - 1;
            resWeek = week + weeksInYear(resYear, dow, doy);
        } else if (week > weeksInYear(mom.year(), dow, doy)) {
            resWeek = week - weeksInYear(mom.year(), dow, doy);
            resYear = mom.year() + 1;
        } else {
            resYear = mom.year();
            resWeek = week;
        }

        return {
            week: resWeek,
            year: resYear,
        };
    }

    function weeksInYear(year, dow, doy) {
        var weekOffset = firstWeekOffset(year, dow, doy),
            weekOffsetNext = firstWeekOffset(year + 1, dow, doy);
        return (daysInYear(year) - weekOffset + weekOffsetNext) / 7;
    }

    // FORMATTING

    addFormatToken('w', ['ww', 2], 'wo', 'week');
    addFormatToken('W', ['WW', 2], 'Wo', 'isoWeek');

    // ALIASES

    addUnitAlias('week', 'w');
    addUnitAlias('isoWeek', 'W');

    // PRIORITIES

    addUnitPriority('week', 5);
    addUnitPriority('isoWeek', 5);

    // PARSING

    addRegexToken('w', match1to2);
    addRegexToken('ww', match1to2, match2);
    addRegexToken('W', match1to2);
    addRegexToken('WW', match1to2, match2);

    addWeekParseToken(['w', 'ww', 'W', 'WW'], function (
        input,
        week,
        config,
        token
    ) {
        week[token.substr(0, 1)] = toInt(input);
    });

    // HELPERS

    // LOCALES

    function localeWeek(mom) {
        return weekOfYear(mom, this._week.dow, this._week.doy).week;
    }

    var defaultLocaleWeek = {
        dow: 0, // Sunday is the first day of the week.
        doy: 6, // The week that contains Jan 6th is the first week of the year.
    };

    function localeFirstDayOfWeek() {
        return this._week.dow;
    }

    function localeFirstDayOfYear() {
        return this._week.doy;
    }

    // MOMENTS

    function getSetWeek(input) {
        var week = this.localeData().week(this);
        return input == null ? week : this.add((input - week) * 7, 'd');
    }

    function getSetISOWeek(input) {
        var week = weekOfYear(this, 1, 4).week;
        return input == null ? week : this.add((input - week) * 7, 'd');
    }

    // FORMATTING

    addFormatToken('d', 0, 'do', 'day');

    addFormatToken('dd', 0, 0, function (format) {
        return this.localeData().weekdaysMin(this, format);
    });

    addFormatToken('ddd', 0, 0, function (format) {
        return this.localeData().weekdaysShort(this, format);
    });

    addFormatToken('dddd', 0, 0, function (format) {
        return this.localeData().weekdays(this, format);
    });

    addFormatToken('e', 0, 0, 'weekday');
    addFormatToken('E', 0, 0, 'isoWeekday');

    // ALIASES

    addUnitAlias('day', 'd');
    addUnitAlias('weekday', 'e');
    addUnitAlias('isoWeekday', 'E');

    // PRIORITY
    addUnitPriority('day', 11);
    addUnitPriority('weekday', 11);
    addUnitPriority('isoWeekday', 11);

    // PARSING

    addRegexToken('d', match1to2);
    addRegexToken('e', match1to2);
    addRegexToken('E', match1to2);
    addRegexToken('dd', function (isStrict, locale) {
        return locale.weekdaysMinRegex(isStrict);
    });
    addRegexToken('ddd', function (isStrict, locale) {
        return locale.weekdaysShortRegex(isStrict);
    });
    addRegexToken('dddd', function (isStrict, locale) {
        return locale.weekdaysRegex(isStrict);
    });

    addWeekParseToken(['dd', 'ddd', 'dddd'], function (input, week, config, token) {
        var weekday = config._locale.weekdaysParse(input, token, config._strict);
        // if we didn't get a weekday name, mark the date as invalid
        if (weekday != null) {
            week.d = weekday;
        } else {
            getParsingFlags(config).invalidWeekday = input;
        }
    });

    addWeekParseToken(['d', 'e', 'E'], function (input, week, config, token) {
        week[token] = toInt(input);
    });

    // HELPERS

    function parseWeekday(input, locale) {
        if (typeof input !== 'string') {
            return input;
        }

        if (!isNaN(input)) {
            return parseInt(input, 10);
        }

        input = locale.weekdaysParse(input);
        if (typeof input === 'number') {
            return input;
        }

        return null;
    }

    function parseIsoWeekday(input, locale) {
        if (typeof input === 'string') {
            return locale.weekdaysParse(input) % 7 || 7;
        }
        return isNaN(input) ? null : input;
    }

    // LOCALES
    function shiftWeekdays(ws, n) {
        return ws.slice(n, 7).concat(ws.slice(0, n));
    }

    var defaultLocaleWeekdays = 'Sunday_Monday_Tuesday_Wednesday_Thursday_Friday_Saturday'.split(
            '_'
        ),
        defaultLocaleWeekdaysShort = 'Sun_Mon_Tue_Wed_Thu_Fri_Sat'.split('_'),
        defaultLocaleWeekdaysMin = 'Su_Mo_Tu_We_Th_Fr_Sa'.split('_'),
        defaultWeekdaysRegex = matchWord,
        defaultWeekdaysShortRegex = matchWord,
        defaultWeekdaysMinRegex = matchWord;

    function localeWeekdays(m, format) {
        var weekdays = isArray(this._weekdays)
            ? this._weekdays
            : this._weekdays[
                  m && m !== true && this._weekdays.isFormat.test(format)
                      ? 'format'
                      : 'standalone'
              ];
        return m === true
            ? shiftWeekdays(weekdays, this._week.dow)
            : m
            ? weekdays[m.day()]
            : weekdays;
    }

    function localeWeekdaysShort(m) {
        return m === true
            ? shiftWeekdays(this._weekdaysShort, this._week.dow)
            : m
            ? this._weekdaysShort[m.day()]
            : this._weekdaysShort;
    }

    function localeWeekdaysMin(m) {
        return m === true
            ? shiftWeekdays(this._weekdaysMin, this._week.dow)
            : m
            ? this._weekdaysMin[m.day()]
            : this._weekdaysMin;
    }

    function handleStrictParse$1(weekdayName, format, strict) {
        var i,
            ii,
            mom,
            llc = weekdayName.toLocaleLowerCase();
        if (!this._weekdaysParse) {
            this._weekdaysParse = [];
            this._shortWeekdaysParse = [];
            this._minWeekdaysParse = [];

            for (i = 0; i < 7; ++i) {
                mom = createUTC([2000, 1]).day(i);
                this._minWeekdaysParse[i] = this.weekdaysMin(
                    mom,
                    ''
                ).toLocaleLowerCase();
                this._shortWeekdaysParse[i] = this.weekdaysShort(
                    mom,
                    ''
                ).toLocaleLowerCase();
                this._weekdaysParse[i] = this.weekdays(mom, '').toLocaleLowerCase();
            }
        }

        if (strict) {
            if (format === 'dddd') {
                ii = indexOf.call(this._weekdaysParse, llc);
                return ii !== -1 ? ii : null;
            } else if (format === 'ddd') {
                ii = indexOf.call(this._shortWeekdaysParse, llc);
                return ii !== -1 ? ii : null;
            } else {
                ii = indexOf.call(this._minWeekdaysParse, llc);
                return ii !== -1 ? ii : null;
            }
        } else {
            if (format === 'dddd') {
                ii = indexOf.call(this._weekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._shortWeekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._minWeekdaysParse, llc);
                return ii !== -1 ? ii : null;
            } else if (format === 'ddd') {
                ii = indexOf.call(this._shortWeekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._weekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._minWeekdaysParse, llc);
                return ii !== -1 ? ii : null;
            } else {
                ii = indexOf.call(this._minWeekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._weekdaysParse, llc);
                if (ii !== -1) {
                    return ii;
                }
                ii = indexOf.call(this._shortWeekdaysParse, llc);
                return ii !== -1 ? ii : null;
            }
        }
    }

    function localeWeekdaysParse(weekdayName, format, strict) {
        var i, mom, regex;

        if (this._weekdaysParseExact) {
            return handleStrictParse$1.call(this, weekdayName, format, strict);
        }

        if (!this._weekdaysParse) {
            this._weekdaysParse = [];
            this._minWeekdaysParse = [];
            this._shortWeekdaysParse = [];
            this._fullWeekdaysParse = [];
        }

        for (i = 0; i < 7; i++) {
            // make the regex if we don't have it already

            mom = createUTC([2000, 1]).day(i);
            if (strict && !this._fullWeekdaysParse[i]) {
                this._fullWeekdaysParse[i] = new RegExp(
                    '^' + this.weekdays(mom, '').replace('.', '\\.?') + '$',
                    'i'
                );
                this._shortWeekdaysParse[i] = new RegExp(
                    '^' + this.weekdaysShort(mom, '').replace('.', '\\.?') + '$',
                    'i'
                );
                this._minWeekdaysParse[i] = new RegExp(
                    '^' + this.weekdaysMin(mom, '').replace('.', '\\.?') + '$',
                    'i'
                );
            }
            if (!this._weekdaysParse[i]) {
                regex =
                    '^' +
                    this.weekdays(mom, '') +
                    '|^' +
                    this.weekdaysShort(mom, '') +
                    '|^' +
                    this.weekdaysMin(mom, '');
                this._weekdaysParse[i] = new RegExp(regex.replace('.', ''), 'i');
            }
            // test the regex
            if (
                strict &&
                format === 'dddd' &&
                this._fullWeekdaysParse[i].test(weekdayName)
            ) {
                return i;
            } else if (
                strict &&
                format === 'ddd' &&
                this._shortWeekdaysParse[i].test(weekdayName)
            ) {
                return i;
            } else if (
                strict &&
                format === 'dd' &&
                this._minWeekdaysParse[i].test(weekdayName)
            ) {
                return i;
            } else if (!strict && this._weekdaysParse[i].test(weekdayName)) {
                return i;
            }
        }
    }

    // MOMENTS

    function getSetDayOfWeek(input) {
        if (!this.isValid()) {
            return input != null ? this : NaN;
        }
        var day = this._isUTC ? this._d.getUTCDay() : this._d.getDay();
        if (input != null) {
            input = parseWeekday(input, this.localeData());
            return this.add(input - day, 'd');
        } else {
            return day;
        }
    }

    function getSetLocaleDayOfWeek(input) {
        if (!this.isValid()) {
            return input != null ? this : NaN;
        }
        var weekday = (this.day() + 7 - this.localeData()._week.dow) % 7;
        return input == null ? weekday : this.add(input - weekday, 'd');
    }

    function getSetISODayOfWeek(input) {
        if (!this.isValid()) {
            return input != null ? this : NaN;
        }

        // behaves the same as moment#day except
        // as a getter, returns 7 instead of 0 (1-7 range instead of 0-6)
        // as a setter, sunday should belong to the previous week.

        if (input != null) {
            var weekday = parseIsoWeekday(input, this.localeData());
            return this.day(this.day() % 7 ? weekday : weekday - 7);
        } else {
            return this.day() || 7;
        }
    }

    function weekdaysRegex(isStrict) {
        if (this._weekdaysParseExact) {
            if (!hasOwnProp(this, '_weekdaysRegex')) {
                computeWeekdaysParse.call(this);
            }
            if (isStrict) {
                return this._weekdaysStrictRegex;
            } else {
                return this._weekdaysRegex;
            }
        } else {
            if (!hasOwnProp(this, '_weekdaysRegex')) {
                this._weekdaysRegex = defaultWeekdaysRegex;
            }
            return this._weekdaysStrictRegex && isStrict
                ? this._weekdaysStrictRegex
                : this._weekdaysRegex;
        }
    }

    function weekdaysShortRegex(isStrict) {
        if (this._weekdaysParseExact) {
            if (!hasOwnProp(this, '_weekdaysRegex')) {
                computeWeekdaysParse.call(this);
            }
            if (isStrict) {
                return this._weekdaysShortStrictRegex;
            } else {
                return this._weekdaysShortRegex;
            }
        } else {
            if (!hasOwnProp(this, '_weekdaysShortRegex')) {
                this._weekdaysShortRegex = defaultWeekdaysShortRegex;
            }
            return this._weekdaysShortStrictRegex && isStrict
                ? this._weekdaysShortStrictRegex
                : this._weekdaysShortRegex;
        }
    }

    function weekdaysMinRegex(isStrict) {
        if (this._weekdaysParseExact) {
            if (!hasOwnProp(this, '_weekdaysRegex')) {
                computeWeekdaysParse.call(this);
            }
            if (isStrict) {
                return this._weekdaysMinStrictRegex;
            } else {
                return this._weekdaysMinRegex;
            }
        } else {
            if (!hasOwnProp(this, '_weekdaysMinRegex')) {
                this._weekdaysMinRegex = defaultWeekdaysMinRegex;
            }
            return this._weekdaysMinStrictRegex && isStrict
                ? this._weekdaysMinStrictRegex
                : this._weekdaysMinRegex;
        }
    }

    function computeWeekdaysParse() {
        function cmpLenRev(a, b) {
            return b.length - a.length;
        }

        var minPieces = [],
            shortPieces = [],
            longPieces = [],
            mixedPieces = [],
            i,
            mom,
            minp,
            shortp,
            longp;
        for (i = 0; i < 7; i++) {
            // make the regex if we don't have it already
            mom = createUTC([2000, 1]).day(i);
            minp = regexEscape(this.weekdaysMin(mom, ''));
            shortp = regexEscape(this.weekdaysShort(mom, ''));
            longp = regexEscape(this.weekdays(mom, ''));
            minPieces.push(minp);
            shortPieces.push(shortp);
            longPieces.push(longp);
            mixedPieces.push(minp);
            mixedPieces.push(shortp);
            mixedPieces.push(longp);
        }
        // Sorting makes sure if one weekday (or abbr) is a prefix of another it
        // will match the longer piece.
        minPieces.sort(cmpLenRev);
        shortPieces.sort(cmpLenRev);
        longPieces.sort(cmpLenRev);
        mixedPieces.sort(cmpLenRev);

        this._weekdaysRegex = new RegExp('^(' + mixedPieces.join('|') + ')', 'i');
        this._weekdaysShortRegex = this._weekdaysRegex;
        this._weekdaysMinRegex = this._weekdaysRegex;

        this._weekdaysStrictRegex = new RegExp(
            '^(' + longPieces.join('|') + ')',
            'i'
        );
        this._weekdaysShortStrictRegex = new RegExp(
            '^(' + shortPieces.join('|') + ')',
            'i'
        );
        this._weekdaysMinStrictRegex = new RegExp(
            '^(' + minPieces.join('|') + ')',
            'i'
        );
    }

    // FORMATTING

    function hFormat() {
        return this.hours() % 12 || 12;
    }

    function kFormat() {
        return this.hours() || 24;
    }

    addFormatToken('H', ['HH', 2], 0, 'hour');
    addFormatToken('h', ['hh', 2], 0, hFormat);
    addFormatToken('k', ['kk', 2], 0, kFormat);

    addFormatToken('hmm', 0, 0, function () {
        return '' + hFormat.apply(this) + zeroFill(this.minutes(), 2);
    });

    addFormatToken('hmmss', 0, 0, function () {
        return (
            '' +
            hFormat.apply(this) +
            zeroFill(this.minutes(), 2) +
            zeroFill(this.seconds(), 2)
        );
    });

    addFormatToken('Hmm', 0, 0, function () {
        return '' + this.hours() + zeroFill(this.minutes(), 2);
    });

    addFormatToken('Hmmss', 0, 0, function () {
        return (
            '' +
            this.hours() +
            zeroFill(this.minutes(), 2) +
            zeroFill(this.seconds(), 2)
        );
    });

    function meridiem(token, lowercase) {
        addFormatToken(token, 0, 0, function () {
            return this.localeData().meridiem(
                this.hours(),
                this.minutes(),
                lowercase
            );
        });
    }

    meridiem('a', true);
    meridiem('A', false);

    // ALIASES

    addUnitAlias('hour', 'h');

    // PRIORITY
    addUnitPriority('hour', 13);

    // PARSING

    function matchMeridiem(isStrict, locale) {
        return locale._meridiemParse;
    }

    addRegexToken('a', matchMeridiem);
    addRegexToken('A', matchMeridiem);
    addRegexToken('H', match1to2);
    addRegexToken('h', match1to2);
    addRegexToken('k', match1to2);
    addRegexToken('HH', match1to2, match2);
    addRegexToken('hh', match1to2, match2);
    addRegexToken('kk', match1to2, match2);

    addRegexToken('hmm', match3to4);
    addRegexToken('hmmss', match5to6);
    addRegexToken('Hmm', match3to4);
    addRegexToken('Hmmss', match5to6);

    addParseToken(['H', 'HH'], HOUR);
    addParseToken(['k', 'kk'], function (input, array, config) {
        var kInput = toInt(input);
        array[HOUR] = kInput === 24 ? 0 : kInput;
    });
    addParseToken(['a', 'A'], function (input, array, config) {
        config._isPm = config._locale.isPM(input);
        config._meridiem = input;
    });
    addParseToken(['h', 'hh'], function (input, array, config) {
        array[HOUR] = toInt(input);
        getParsingFlags(config).bigHour = true;
    });
    addParseToken('hmm', function (input, array, config) {
        var pos = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos));
        array[MINUTE] = toInt(input.substr(pos));
        getParsingFlags(config).bigHour = true;
    });
    addParseToken('hmmss', function (input, array, config) {
        var pos1 = input.length - 4,
            pos2 = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos1));
        array[MINUTE] = toInt(input.substr(pos1, 2));
        array[SECOND] = toInt(input.substr(pos2));
        getParsingFlags(config).bigHour = true;
    });
    addParseToken('Hmm', function (input, array, config) {
        var pos = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos));
        array[MINUTE] = toInt(input.substr(pos));
    });
    addParseToken('Hmmss', function (input, array, config) {
        var pos1 = input.length - 4,
            pos2 = input.length - 2;
        array[HOUR] = toInt(input.substr(0, pos1));
        array[MINUTE] = toInt(input.substr(pos1, 2));
        array[SECOND] = toInt(input.substr(pos2));
    });

    // LOCALES

    function localeIsPM(input) {
        // IE8 Quirks Mode & IE7 Standards Mode do not allow accessing strings like arrays
        // Using charAt should be more compatible.
        return (input + '').toLowerCase().charAt(0) === 'p';
    }

    var defaultLocaleMeridiemParse = /[ap]\.?m?\.?/i,
        // Setting the hour should keep the time, because the user explicitly
        // specified which hour they want. So trying to maintain the same hour (in
        // a new timezone) makes sense. Adding/subtracting hours does not follow
        // this rule.
        getSetHour = makeGetSet('Hours', true);

    function localeMeridiem(hours, minutes, isLower) {
        if (hours > 11) {
            return isLower ? 'pm' : 'PM';
        } else {
            return isLower ? 'am' : 'AM';
        }
    }

    var baseConfig = {
        calendar: defaultCalendar,
        longDateFormat: defaultLongDateFormat,
        invalidDate: defaultInvalidDate,
        ordinal: defaultOrdinal,
        dayOfMonthOrdinalParse: defaultDayOfMonthOrdinalParse,
        relativeTime: defaultRelativeTime,

        months: defaultLocaleMonths,
        monthsShort: defaultLocaleMonthsShort,

        week: defaultLocaleWeek,

        weekdays: defaultLocaleWeekdays,
        weekdaysMin: defaultLocaleWeekdaysMin,
        weekdaysShort: defaultLocaleWeekdaysShort,

        meridiemParse: defaultLocaleMeridiemParse,
    };

    // internal storage for locale config files
    var locales = {},
        localeFamilies = {},
        globalLocale;

    function commonPrefix(arr1, arr2) {
        var i,
            minl = Math.min(arr1.length, arr2.length);
        for (i = 0; i < minl; i += 1) {
            if (arr1[i] !== arr2[i]) {
                return i;
            }
        }
        return minl;
    }

    function normalizeLocale(key) {
        return key ? key.toLowerCase().replace('_', '-') : key;
    }

    // pick the locale from the array
    // try ['en-au', 'en-gb'] as 'en-au', 'en-gb', 'en', as in move through the list trying each
    // substring from most specific to least, but move to the next array item if it's a more specific variant than the current root
    function chooseLocale(names) {
        var i = 0,
            j,
            next,
            locale,
            split;

        while (i < names.length) {
            split = normalizeLocale(names[i]).split('-');
            j = split.length;
            next = normalizeLocale(names[i + 1]);
            next = next ? next.split('-') : null;
            while (j > 0) {
                locale = loadLocale(split.slice(0, j).join('-'));
                if (locale) {
                    return locale;
                }
                if (
                    next &&
                    next.length >= j &&
                    commonPrefix(split, next) >= j - 1
                ) {
                    //the next array item is better than a shallower substring of this one
                    break;
                }
                j--;
            }
            i++;
        }
        return globalLocale;
    }

    function loadLocale(name) {
        var oldLocale = null,
            aliasedRequire;
        // TODO: Find a better way to register and load all the locales in Node
        if (
            locales[name] === undefined &&
            typeof module !== 'undefined' &&
            module &&
            module.exports
        ) {
            try {
                oldLocale = globalLocale._abbr;
                aliasedRequire = require;
                aliasedRequire('./locale/' + name);
                getSetGlobalLocale(oldLocale);
            } catch (e) {
                // mark as not found to avoid repeating expensive file require call causing high CPU
                // when trying to find en-US, en_US, en-us for every format call
                locales[name] = null; // null means not found
            }
        }
        return locales[name];
    }

    // This function will load locale and then set the global locale.  If
    // no arguments are passed in, it will simply return the current global
    // locale key.
    function getSetGlobalLocale(key, values) {
        var data;
        if (key) {
            if (isUndefined(values)) {
                data = getLocale(key);
            } else {
                data = defineLocale(key, values);
            }

            if (data) {
                // moment.duration._locale = moment._locale = data;
                globalLocale = data;
            } else {
                if (typeof console !== 'undefined' && console.warn) {
                    //warn user if arguments are passed but the locale could not be set
                    console.warn(
                        'Locale ' + key + ' not found. Did you forget to load it?'
                    );
                }
            }
        }

        return globalLocale._abbr;
    }

    function defineLocale(name, config) {
        if (config !== null) {
            var locale,
                parentConfig = baseConfig;
            config.abbr = name;
            if (locales[name] != null) {
                deprecateSimple(
                    'defineLocaleOverride',
                    'use moment.updateLocale(localeName, config) to change ' +
                        'an existing locale. moment.defineLocale(localeName, ' +
                        'config) should only be used for creating a new locale ' +
                        'See http://momentjs.com/guides/#/warnings/define-locale/ for more info.'
                );
                parentConfig = locales[name]._config;
            } else if (config.parentLocale != null) {
                if (locales[config.parentLocale] != null) {
                    parentConfig = locales[config.parentLocale]._config;
                } else {
                    locale = loadLocale(config.parentLocale);
                    if (locale != null) {
                        parentConfig = locale._config;
                    } else {
                        if (!localeFamilies[config.parentLocale]) {
                            localeFamilies[config.parentLocale] = [];
                        }
                        localeFamilies[config.parentLocale].push({
                            name: name,
                            config: config,
                        });
                        return null;
                    }
                }
            }
            locales[name] = new Locale(mergeConfigs(parentConfig, config));

            if (localeFamilies[name]) {
                localeFamilies[name].forEach(function (x) {
                    defineLocale(x.name, x.config);
                });
            }

            // backwards compat for now: also set the locale
            // make sure we set the locale AFTER all child locales have been
            // created, so we won't end up with the child locale set.
            getSetGlobalLocale(name);

            return locales[name];
        } else {
            // useful for testing
            delete locales[name];
            return null;
        }
    }

    function updateLocale(name, config) {
        if (config != null) {
            var locale,
                tmpLocale,
                parentConfig = baseConfig;

            if (locales[name] != null && locales[name].parentLocale != null) {
                // Update existing child locale in-place to avoid memory-leaks
                locales[name].set(mergeConfigs(locales[name]._config, config));
            } else {
                // MERGE
                tmpLocale = loadLocale(name);
                if (tmpLocale != null) {
                    parentConfig = tmpLocale._config;
                }
                config = mergeConfigs(parentConfig, config);
                if (tmpLocale == null) {
                    // updateLocale is called for creating a new locale
                    // Set abbr so it will have a name (getters return
                    // undefined otherwise).
                    config.abbr = name;
                }
                locale = new Locale(config);
                locale.parentLocale = locales[name];
                locales[name] = locale;
            }

            // backwards compat for now: also set the locale
            getSetGlobalLocale(name);
        } else {
            // pass null for config to unupdate, useful for tests
            if (locales[name] != null) {
                if (locales[name].parentLocale != null) {
                    locales[name] = locales[name].parentLocale;
                    if (name === getSetGlobalLocale()) {
                        getSetGlobalLocale(name);
                    }
                } else if (locales[name] != null) {
                    delete locales[name];
                }
            }
        }
        return locales[name];
    }

    // returns locale data
    function getLocale(key) {
        var locale;

        if (key && key._locale && key._locale._abbr) {
            key = key._locale._abbr;
        }

        if (!key) {
            return globalLocale;
        }

        if (!isArray(key)) {
            //short-circuit everything else
            locale = loadLocale(key);
            if (locale) {
                return locale;
            }
            key = [key];
        }

        return chooseLocale(key);
    }

    function listLocales() {
        return keys(locales);
    }

    function checkOverflow(m) {
        var overflow,
            a = m._a;

        if (a && getParsingFlags(m).overflow === -2) {
            overflow =
                a[MONTH] < 0 || a[MONTH] > 11
                    ? MONTH
                    : a[DATE] < 1 || a[DATE] > daysInMonth(a[YEAR], a[MONTH])
                    ? DATE
                    : a[HOUR] < 0 ||
                      a[HOUR] > 24 ||
                      (a[HOUR] === 24 &&
                          (a[MINUTE] !== 0 ||
                              a[SECOND] !== 0 ||
                              a[MILLISECOND] !== 0))
                    ? HOUR
                    : a[MINUTE] < 0 || a[MINUTE] > 59
                    ? MINUTE
                    : a[SECOND] < 0 || a[SECOND] > 59
                    ? SECOND
                    : a[MILLISECOND] < 0 || a[MILLISECOND] > 999
                    ? MILLISECOND
                    : -1;

            if (
                getParsingFlags(m)._overflowDayOfYear &&
                (overflow < YEAR || overflow > DATE)
            ) {
                overflow = DATE;
            }
            if (getParsingFlags(m)._overflowWeeks && overflow === -1) {
                overflow = WEEK;
            }
            if (getParsingFlags(m)._overflowWeekday && overflow === -1) {
                overflow = WEEKDAY;
            }

            getParsingFlags(m).overflow = overflow;
        }

        return m;
    }

    // iso 8601 regex
    // 0000-00-00 0000-W00 or 0000-W00-0 + T + 00 or 00:00 or 00:00:00 or 00:00:00.000 + +00:00 or +0000 or +00)
    var extendedIsoRegex = /^\s*((?:[+-]\d{6}|\d{4})-(?:\d\d-\d\d|W\d\d-\d|W\d\d|\d\d\d|\d\d))(?:(T| )(\d\d(?::\d\d(?::\d\d(?:[.,]\d+)?)?)?)([+-]\d\d(?::?\d\d)?|\s*Z)?)?$/,
        basicIsoRegex = /^\s*((?:[+-]\d{6}|\d{4})(?:\d\d\d\d|W\d\d\d|W\d\d|\d\d\d|\d\d|))(?:(T| )(\d\d(?:\d\d(?:\d\d(?:[.,]\d+)?)?)?)([+-]\d\d(?::?\d\d)?|\s*Z)?)?$/,
        tzRegex = /Z|[+-]\d\d(?::?\d\d)?/,
        isoDates = [
            ['YYYYYY-MM-DD', /[+-]\d{6}-\d\d-\d\d/],
            ['YYYY-MM-DD', /\d{4}-\d\d-\d\d/],
            ['GGGG-[W]WW-E', /\d{4}-W\d\d-\d/],
            ['GGGG-[W]WW', /\d{4}-W\d\d/, false],
            ['YYYY-DDD', /\d{4}-\d{3}/],
            ['YYYY-MM', /\d{4}-\d\d/, false],
            ['YYYYYYMMDD', /[+-]\d{10}/],
            ['YYYYMMDD', /\d{8}/],
            ['GGGG[W]WWE', /\d{4}W\d{3}/],
            ['GGGG[W]WW', /\d{4}W\d{2}/, false],
            ['YYYYDDD', /\d{7}/],
            ['YYYYMM', /\d{6}/, false],
            ['YYYY', /\d{4}/, false],
        ],
        // iso time formats and regexes
        isoTimes = [
            ['HH:mm:ss.SSSS', /\d\d:\d\d:\d\d\.\d+/],
            ['HH:mm:ss,SSSS', /\d\d:\d\d:\d\d,\d+/],
            ['HH:mm:ss', /\d\d:\d\d:\d\d/],
            ['HH:mm', /\d\d:\d\d/],
            ['HHmmss.SSSS', /\d\d\d\d\d\d\.\d+/],
            ['HHmmss,SSSS', /\d\d\d\d\d\d,\d+/],
            ['HHmmss', /\d\d\d\d\d\d/],
            ['HHmm', /\d\d\d\d/],
            ['HH', /\d\d/],
        ],
        aspNetJsonRegex = /^\/?Date\((-?\d+)/i,
        // RFC 2822 regex: For details see https://tools.ietf.org/html/rfc2822#section-3.3
        rfc2822 = /^(?:(Mon|Tue|Wed|Thu|Fri|Sat|Sun),?\s)?(\d{1,2})\s(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\s(\d{2,4})\s(\d\d):(\d\d)(?::(\d\d))?\s(?:(UT|GMT|[ECMP][SD]T)|([Zz])|([+-]\d{4}))$/,
        obsOffsets = {
            UT: 0,
            GMT: 0,
            EDT: -4 * 60,
            EST: -5 * 60,
            CDT: -5 * 60,
            CST: -6 * 60,
            MDT: -6 * 60,
            MST: -7 * 60,
            PDT: -7 * 60,
            PST: -8 * 60,
        };

    // date from iso format
    function configFromISO(config) {
        var i,
            l,
            string = config._i,
            match = extendedIsoRegex.exec(string) || basicIsoRegex.exec(string),
            allowTime,
            dateFormat,
            timeFormat,
            tzFormat;

        if (match) {
            getParsingFlags(config).iso = true;

            for (i = 0, l = isoDates.length; i < l; i++) {
                if (isoDates[i][1].exec(match[1])) {
                    dateFormat = isoDates[i][0];
                    allowTime = isoDates[i][2] !== false;
                    break;
                }
            }
            if (dateFormat == null) {
                config._isValid = false;
                return;
            }
            if (match[3]) {
                for (i = 0, l = isoTimes.length; i < l; i++) {
                    if (isoTimes[i][1].exec(match[3])) {
                        // match[2] should be 'T' or space
                        timeFormat = (match[2] || ' ') + isoTimes[i][0];
                        break;
                    }
                }
                if (timeFormat == null) {
                    config._isValid = false;
                    return;
                }
            }
            if (!allowTime && timeFormat != null) {
                config._isValid = false;
                return;
            }
            if (match[4]) {
                if (tzRegex.exec(match[4])) {
                    tzFormat = 'Z';
                } else {
                    config._isValid = false;
                    return;
                }
            }
            config._f = dateFormat + (timeFormat || '') + (tzFormat || '');
            configFromStringAndFormat(config);
        } else {
            config._isValid = false;
        }
    }

    function extractFromRFC2822Strings(
        yearStr,
        monthStr,
        dayStr,
        hourStr,
        minuteStr,
        secondStr
    ) {
        var result = [
            untruncateYear(yearStr),
            defaultLocaleMonthsShort.indexOf(monthStr),
            parseInt(dayStr, 10),
            parseInt(hourStr, 10),
            parseInt(minuteStr, 10),
        ];

        if (secondStr) {
            result.push(parseInt(secondStr, 10));
        }

        return result;
    }

    function untruncateYear(yearStr) {
        var year = parseInt(yearStr, 10);
        if (year <= 49) {
            return 2000 + year;
        } else if (year <= 999) {
            return 1900 + year;
        }
        return year;
    }

    function preprocessRFC2822(s) {
        // Remove comments and folding whitespace and replace multiple-spaces with a single space
        return s
            .replace(/\([^)]*\)|[\n\t]/g, ' ')
            .replace(/(\s\s+)/g, ' ')
            .replace(/^\s\s*/, '')
            .replace(/\s\s*$/, '');
    }

    function checkWeekday(weekdayStr, parsedInput, config) {
        if (weekdayStr) {
            // TODO: Replace the vanilla JS Date object with an independent day-of-week check.
            var weekdayProvided = defaultLocaleWeekdaysShort.indexOf(weekdayStr),
                weekdayActual = new Date(
                    parsedInput[0],
                    parsedInput[1],
                    parsedInput[2]
                ).getDay();
            if (weekdayProvided !== weekdayActual) {
                getParsingFlags(config).weekdayMismatch = true;
                config._isValid = false;
                return false;
            }
        }
        return true;
    }

    function calculateOffset(obsOffset, militaryOffset, numOffset) {
        if (obsOffset) {
            return obsOffsets[obsOffset];
        } else if (militaryOffset) {
            // the only allowed military tz is Z
            return 0;
        } else {
            var hm = parseInt(numOffset, 10),
                m = hm % 100,
                h = (hm - m) / 100;
            return h * 60 + m;
        }
    }

    // date and time from ref 2822 format
    function configFromRFC2822(config) {
        var match = rfc2822.exec(preprocessRFC2822(config._i)),
            parsedArray;
        if (match) {
            parsedArray = extractFromRFC2822Strings(
                match[4],
                match[3],
                match[2],
                match[5],
                match[6],
                match[7]
            );
            if (!checkWeekday(match[1], parsedArray, config)) {
                return;
            }

            config._a = parsedArray;
            config._tzm = calculateOffset(match[8], match[9], match[10]);

            config._d = createUTCDate.apply(null, config._a);
            config._d.setUTCMinutes(config._d.getUTCMinutes() - config._tzm);

            getParsingFlags(config).rfc2822 = true;
        } else {
            config._isValid = false;
        }
    }

    // date from 1) ASP.NET, 2) ISO, 3) RFC 2822 formats, or 4) optional fallback if parsing isn't strict
    function configFromString(config) {
        var matched = aspNetJsonRegex.exec(config._i);
        if (matched !== null) {
            config._d = new Date(+matched[1]);
            return;
        }

        configFromISO(config);
        if (config._isValid === false) {
            delete config._isValid;
        } else {
            return;
        }

        configFromRFC2822(config);
        if (config._isValid === false) {
            delete config._isValid;
        } else {
            return;
        }

        if (config._strict) {
            config._isValid = false;
        } else {
            // Final attempt, use Input Fallback
            hooks.createFromInputFallback(config);
        }
    }

    hooks.createFromInputFallback = deprecate(
        'value provided is not in a recognized RFC2822 or ISO format. moment construction falls back to js Date(), ' +
            'which is not reliable across all browsers and versions. Non RFC2822/ISO date formats are ' +
            'discouraged. Please refer to http://momentjs.com/guides/#/warnings/js-date/ for more info.',
        function (config) {
            config._d = new Date(config._i + (config._useUTC ? ' UTC' : ''));
        }
    );

    // Pick the first defined of two or three arguments.
    function defaults(a, b, c) {
        if (a != null) {
            return a;
        }
        if (b != null) {
            return b;
        }
        return c;
    }

    function currentDateArray(config) {
        // hooks is actually the exported moment object
        var nowValue = new Date(hooks.now());
        if (config._useUTC) {
            return [
                nowValue.getUTCFullYear(),
                nowValue.getUTCMonth(),
                nowValue.getUTCDate(),
            ];
        }
        return [nowValue.getFullYear(), nowValue.getMonth(), nowValue.getDate()];
    }

    // convert an array to a date.
    // the array should mirror the parameters below
    // note: all values past the year are optional and will default to the lowest possible value.
    // [year, month, day , hour, minute, second, millisecond]
    function configFromArray(config) {
        var i,
            date,
            input = [],
            currentDate,
            expectedWeekday,
            yearToUse;

        if (config._d) {
            return;
        }

        currentDate = currentDateArray(config);

        //compute day of the year from weeks and weekdays
        if (config._w && config._a[DATE] == null && config._a[MONTH] == null) {
            dayOfYearFromWeekInfo(config);
        }

        //if the day of the year is set, figure out what it is
        if (config._dayOfYear != null) {
            yearToUse = defaults(config._a[YEAR], currentDate[YEAR]);

            if (
                config._dayOfYear > daysInYear(yearToUse) ||
                config._dayOfYear === 0
            ) {
                getParsingFlags(config)._overflowDayOfYear = true;
            }

            date = createUTCDate(yearToUse, 0, config._dayOfYear);
            config._a[MONTH] = date.getUTCMonth();
            config._a[DATE] = date.getUTCDate();
        }

        // Default to current date.
        // * if no year, month, day of month are given, default to today
        // * if day of month is given, default month and year
        // * if month is given, default only year
        // * if year is given, don't default anything
        for (i = 0; i < 3 && config._a[i] == null; ++i) {
            config._a[i] = input[i] = currentDate[i];
        }

        // Zero out whatever was not defaulted, including time
        for (; i < 7; i++) {
            config._a[i] = input[i] =
                config._a[i] == null ? (i === 2 ? 1 : 0) : config._a[i];
        }

        // Check for 24:00:00.000
        if (
            config._a[HOUR] === 24 &&
            config._a[MINUTE] === 0 &&
            config._a[SECOND] === 0 &&
            config._a[MILLISECOND] === 0
        ) {
            config._nextDay = true;
            config._a[HOUR] = 0;
        }

        config._d = (config._useUTC ? createUTCDate : createDate).apply(
            null,
            input
        );
        expectedWeekday = config._useUTC
            ? config._d.getUTCDay()
            : config._d.getDay();

        // Apply timezone offset from input. The actual utcOffset can be changed
        // with parseZone.
        if (config._tzm != null) {
            config._d.setUTCMinutes(config._d.getUTCMinutes() - config._tzm);
        }

        if (config._nextDay) {
            config._a[HOUR] = 24;
        }

        // check for mismatching day of week
        if (
            config._w &&
            typeof config._w.d !== 'undefined' &&
            config._w.d !== expectedWeekday
        ) {
            getParsingFlags(config).weekdayMismatch = true;
        }
    }

    function dayOfYearFromWeekInfo(config) {
        var w, weekYear, week, weekday, dow, doy, temp, weekdayOverflow, curWeek;

        w = config._w;
        if (w.GG != null || w.W != null || w.E != null) {
            dow = 1;
            doy = 4;

            // TODO: We need to take the current isoWeekYear, but that depends on
            // how we interpret now (local, utc, fixed offset). So create
            // a now version of current config (take local/utc/offset flags, and
            // create now).
            weekYear = defaults(
                w.GG,
                config._a[YEAR],
                weekOfYear(createLocal(), 1, 4).year
            );
            week = defaults(w.W, 1);
            weekday = defaults(w.E, 1);
            if (weekday < 1 || weekday > 7) {
                weekdayOverflow = true;
            }
        } else {
            dow = config._locale._week.dow;
            doy = config._locale._week.doy;

            curWeek = weekOfYear(createLocal(), dow, doy);

            weekYear = defaults(w.gg, config._a[YEAR], curWeek.year);

            // Default to current week.
            week = defaults(w.w, curWeek.week);

            if (w.d != null) {
                // weekday -- low day numbers are considered next week
                weekday = w.d;
                if (weekday < 0 || weekday > 6) {
                    weekdayOverflow = true;
                }
            } else if (w.e != null) {
                // local weekday -- counting starts from beginning of week
                weekday = w.e + dow;
                if (w.e < 0 || w.e > 6) {
                    weekdayOverflow = true;
                }
            } else {
                // default to beginning of week
                weekday = dow;
            }
        }
        if (week < 1 || week > weeksInYear(weekYear, dow, doy)) {
            getParsingFlags(config)._overflowWeeks = true;
        } else if (weekdayOverflow != null) {
            getParsingFlags(config)._overflowWeekday = true;
        } else {
            temp = dayOfYearFromWeeks(weekYear, week, weekday, dow, doy);
            config._a[YEAR] = temp.year;
            config._dayOfYear = temp.dayOfYear;
        }
    }

    // constant that refers to the ISO standard
    hooks.ISO_8601 = function () {};

    // constant that refers to the RFC 2822 form
    hooks.RFC_2822 = function () {};

    // date from string and format string
    function configFromStringAndFormat(config) {
        // TODO: Move this to another part of the creation flow to prevent circular deps
        if (config._f === hooks.ISO_8601) {
            configFromISO(config);
            return;
        }
        if (config._f === hooks.RFC_2822) {
            configFromRFC2822(config);
            return;
        }
        config._a = [];
        getParsingFlags(config).empty = true;

        // This array is used to make a Date, either with `new Date` or `Date.UTC`
        var string = '' + config._i,
            i,
            parsedInput,
            tokens,
            token,
            skipped,
            stringLength = string.length,
            totalParsedInputLength = 0,
            era;

        tokens =
            expandFormat(config._f, config._locale).match(formattingTokens) || [];

        for (i = 0; i < tokens.length; i++) {
            token = tokens[i];
            parsedInput = (string.match(getParseRegexForToken(token, config)) ||
                [])[0];
            if (parsedInput) {
                skipped = string.substr(0, string.indexOf(parsedInput));
                if (skipped.length > 0) {
                    getParsingFlags(config).unusedInput.push(skipped);
                }
                string = string.slice(
                    string.indexOf(parsedInput) + parsedInput.length
                );
                totalParsedInputLength += parsedInput.length;
            }
            // don't parse if it's not a known token
            if (formatTokenFunctions[token]) {
                if (parsedInput) {
                    getParsingFlags(config).empty = false;
                } else {
                    getParsingFlags(config).unusedTokens.push(token);
                }
                addTimeToArrayFromToken(token, parsedInput, config);
            } else if (config._strict && !parsedInput) {
                getParsingFlags(config).unusedTokens.push(token);
            }
        }

        // add remaining unparsed input length to the string
        getParsingFlags(config).charsLeftOver =
            stringLength - totalParsedInputLength;
        if (string.length > 0) {
            getParsingFlags(config).unusedInput.push(string);
        }

        // clear _12h flag if hour is <= 12
        if (
            config._a[HOUR] <= 12 &&
            getParsingFlags(config).bigHour === true &&
            config._a[HOUR] > 0
        ) {
            getParsingFlags(config).bigHour = undefined;
        }

        getParsingFlags(config).parsedDateParts = config._a.slice(0);
        getParsingFlags(config).meridiem = config._meridiem;
        // handle meridiem
        config._a[HOUR] = meridiemFixWrap(
            config._locale,
            config._a[HOUR],
            config._meridiem
        );

        // handle era
        era = getParsingFlags(config).era;
        if (era !== null) {
            config._a[YEAR] = config._locale.erasConvertYear(era, config._a[YEAR]);
        }

        configFromArray(config);
        checkOverflow(config);
    }

    function meridiemFixWrap(locale, hour, meridiem) {
        var isPm;

        if (meridiem == null) {
            // nothing to do
            return hour;
        }
        if (locale.meridiemHour != null) {
            return locale.meridiemHour(hour, meridiem);
        } else if (locale.isPM != null) {
            // Fallback
            isPm = locale.isPM(meridiem);
            if (isPm && hour < 12) {
                hour += 12;
            }
            if (!isPm && hour === 12) {
                hour = 0;
            }
            return hour;
        } else {
            // this is not supposed to happen
            return hour;
        }
    }

    // date from string and array of format strings
    function configFromStringAndArray(config) {
        var tempConfig,
            bestMoment,
            scoreToBeat,
            i,
            currentScore,
            validFormatFound,
            bestFormatIsValid = false;

        if (config._f.length === 0) {
            getParsingFlags(config).invalidFormat = true;
            config._d = new Date(NaN);
            return;
        }

        for (i = 0; i < config._f.length; i++) {
            currentScore = 0;
            validFormatFound = false;
            tempConfig = copyConfig({}, config);
            if (config._useUTC != null) {
                tempConfig._useUTC = config._useUTC;
            }
            tempConfig._f = config._f[i];
            configFromStringAndFormat(tempConfig);

            if (isValid(tempConfig)) {
                validFormatFound = true;
            }

            // if there is any input that was not parsed add a penalty for that format
            currentScore += getParsingFlags(tempConfig).charsLeftOver;

            //or tokens
            currentScore += getParsingFlags(tempConfig).unusedTokens.length * 10;

            getParsingFlags(tempConfig).score = currentScore;

            if (!bestFormatIsValid) {
                if (
                    scoreToBeat == null ||
                    currentScore < scoreToBeat ||
                    validFormatFound
                ) {
                    scoreToBeat = currentScore;
                    bestMoment = tempConfig;
                    if (validFormatFound) {
                        bestFormatIsValid = true;
                    }
                }
            } else {
                if (currentScore < scoreToBeat) {
                    scoreToBeat = currentScore;
                    bestMoment = tempConfig;
                }
            }
        }

        extend(config, bestMoment || tempConfig);
    }

    function configFromObject(config) {
        if (config._d) {
            return;
        }

        var i = normalizeObjectUnits(config._i),
            dayOrDate = i.day === undefined ? i.date : i.day;
        config._a = map(
            [i.year, i.month, dayOrDate, i.hour, i.minute, i.second, i.millisecond],
            function (obj) {
                return obj && parseInt(obj, 10);
            }
        );

        configFromArray(config);
    }

    function createFromConfig(config) {
        var res = new Moment(checkOverflow(prepareConfig(config)));
        if (res._nextDay) {
            // Adding is smart enough around DST
            res.add(1, 'd');
            res._nextDay = undefined;
        }

        return res;
    }

    function prepareConfig(config) {
        var input = config._i,
            format = config._f;

        config._locale = config._locale || getLocale(config._l);

        if (input === null || (format === undefined && input === '')) {
            return createInvalid({ nullInput: true });
        }

        if (typeof input === 'string') {
            config._i = input = config._locale.preparse(input);
        }

        if (isMoment(input)) {
            return new Moment(checkOverflow(input));
        } else if (isDate(input)) {
            config._d = input;
        } else if (isArray(format)) {
            configFromStringAndArray(config);
        } else if (format) {
            configFromStringAndFormat(config);
        } else {
            configFromInput(config);
        }

        if (!isValid(config)) {
            config._d = null;
        }

        return config;
    }

    function configFromInput(config) {
        var input = config._i;
        if (isUndefined(input)) {
            config._d = new Date(hooks.now());
        } else if (isDate(input)) {
            config._d = new Date(input.valueOf());
        } else if (typeof input === 'string') {
            configFromString(config);
        } else if (isArray(input)) {
            config._a = map(input.slice(0), function (obj) {
                return parseInt(obj, 10);
            });
            configFromArray(config);
        } else if (isObject(input)) {
            configFromObject(config);
        } else if (isNumber(input)) {
            // from milliseconds
            config._d = new Date(input);
        } else {
            hooks.createFromInputFallback(config);
        }
    }

    function createLocalOrUTC(input, format, locale, strict, isUTC) {
        var c = {};

        if (format === true || format === false) {
            strict = format;
            format = undefined;
        }

        if (locale === true || locale === false) {
            strict = locale;
            locale = undefined;
        }

        if (
            (isObject(input) && isObjectEmpty(input)) ||
            (isArray(input) && input.length === 0)
        ) {
            input = undefined;
        }
        // object construction must be done this way.
        // https://github.com/moment/moment/issues/1423
        c._isAMomentObject = true;
        c._useUTC = c._isUTC = isUTC;
        c._l = locale;
        c._i = input;
        c._f = format;
        c._strict = strict;

        return createFromConfig(c);
    }

    function createLocal(input, format, locale, strict) {
        return createLocalOrUTC(input, format, locale, strict, false);
    }

    var prototypeMin = deprecate(
            'moment().min is deprecated, use moment.max instead. http://momentjs.com/guides/#/warnings/min-max/',
            function () {
                var other = createLocal.apply(null, arguments);
                if (this.isValid() && other.isValid()) {
                    return other < this ? this : other;
                } else {
                    return createInvalid();
                }
            }
        ),
        prototypeMax = deprecate(
            'moment().max is deprecated, use moment.min instead. http://momentjs.com/guides/#/warnings/min-max/',
            function () {
                var other = createLocal.apply(null, arguments);
                if (this.isValid() && other.isValid()) {
                    return other > this ? this : other;
                } else {
                    return createInvalid();
                }
            }
        );

    // Pick a moment m from moments so that m[fn](other) is true for all
    // other. This relies on the function fn to be transitive.
    //
    // moments should either be an array of moment objects or an array, whose
    // first element is an array of moment objects.
    function pickBy(fn, moments) {
        var res, i;
        if (moments.length === 1 && isArray(moments[0])) {
            moments = moments[0];
        }
        if (!moments.length) {
            return createLocal();
        }
        res = moments[0];
        for (i = 1; i < moments.length; ++i) {
            if (!moments[i].isValid() || moments[i][fn](res)) {
                res = moments[i];
            }
        }
        return res;
    }

    // TODO: Use [].sort instead?
    function min() {
        var args = [].slice.call(arguments, 0);

        return pickBy('isBefore', args);
    }

    function max() {
        var args = [].slice.call(arguments, 0);

        return pickBy('isAfter', args);
    }

    var now = function () {
        return Date.now ? Date.now() : +new Date();
    };

    var ordering = [
        'year',
        'quarter',
        'month',
        'week',
        'day',
        'hour',
        'minute',
        'second',
        'millisecond',
    ];

    function isDurationValid(m) {
        var key,
            unitHasDecimal = false,
            i;
        for (key in m) {
            if (
                hasOwnProp(m, key) &&
                !(
                    indexOf.call(ordering, key) !== -1 &&
                    (m[key] == null || !isNaN(m[key]))
                )
            ) {
                return false;
            }
        }

        for (i = 0; i < ordering.length; ++i) {
            if (m[ordering[i]]) {
                if (unitHasDecimal) {
                    return false; // only allow non-integers for smallest unit
                }
                if (parseFloat(m[ordering[i]]) !== toInt(m[ordering[i]])) {
                    unitHasDecimal = true;
                }
            }
        }

        return true;
    }

    function isValid$1() {
        return this._isValid;
    }

    function createInvalid$1() {
        return createDuration(NaN);
    }

    function Duration(duration) {
        var normalizedInput = normalizeObjectUnits(duration),
            years = normalizedInput.year || 0,
            quarters = normalizedInput.quarter || 0,
            months = normalizedInput.month || 0,
            weeks = normalizedInput.week || normalizedInput.isoWeek || 0,
            days = normalizedInput.day || 0,
            hours = normalizedInput.hour || 0,
            minutes = normalizedInput.minute || 0,
            seconds = normalizedInput.second || 0,
            milliseconds = normalizedInput.millisecond || 0;

        this._isValid = isDurationValid(normalizedInput);

        // representation for dateAddRemove
        this._milliseconds =
            +milliseconds +
            seconds * 1e3 + // 1000
            minutes * 6e4 + // 1000 * 60
            hours * 1000 * 60 * 60; //using 1000 * 60 * 60 instead of 36e5 to avoid floating point rounding errors https://github.com/moment/moment/issues/2978
        // Because of dateAddRemove treats 24 hours as different from a
        // day when working around DST, we need to store them separately
        this._days = +days + weeks * 7;
        // It is impossible to translate months into days without knowing
        // which months you are are talking about, so we have to store
        // it separately.
        this._months = +months + quarters * 3 + years * 12;

        this._data = {};

        this._locale = getLocale();

        this._bubble();
    }

    function isDuration(obj) {
        return obj instanceof Duration;
    }

    function absRound(number) {
        if (number < 0) {
            return Math.round(-1 * number) * -1;
        } else {
            return Math.round(number);
        }
    }

    // compare two arrays, return the number of differences
    function compareArrays(array1, array2, dontConvert) {
        var len = Math.min(array1.length, array2.length),
            lengthDiff = Math.abs(array1.length - array2.length),
            diffs = 0,
            i;
        for (i = 0; i < len; i++) {
            if (
                (dontConvert && array1[i] !== array2[i]) ||
                (!dontConvert && toInt(array1[i]) !== toInt(array2[i]))
            ) {
                diffs++;
            }
        }
        return diffs + lengthDiff;
    }

    // FORMATTING

    function offset(token, separator) {
        addFormatToken(token, 0, 0, function () {
            var offset = this.utcOffset(),
                sign = '+';
            if (offset < 0) {
                offset = -offset;
                sign = '-';
            }
            return (
                sign +
                zeroFill(~~(offset / 60), 2) +
                separator +
                zeroFill(~~offset % 60, 2)
            );
        });
    }

    offset('Z', ':');
    offset('ZZ', '');

    // PARSING

    addRegexToken('Z', matchShortOffset);
    addRegexToken('ZZ', matchShortOffset);
    addParseToken(['Z', 'ZZ'], function (input, array, config) {
        config._useUTC = true;
        config._tzm = offsetFromString(matchShortOffset, input);
    });

    // HELPERS

    // timezone chunker
    // '+10:00' > ['10',  '00']
    // '-1530'  > ['-15', '30']
    var chunkOffset = /([\+\-]|\d\d)/gi;

    function offsetFromString(matcher, string) {
        var matches = (string || '').match(matcher),
            chunk,
            parts,
            minutes;

        if (matches === null) {
            return null;
        }

        chunk = matches[matches.length - 1] || [];
        parts = (chunk + '').match(chunkOffset) || ['-', 0, 0];
        minutes = +(parts[1] * 60) + toInt(parts[2]);

        return minutes === 0 ? 0 : parts[0] === '+' ? minutes : -minutes;
    }

    // Return a moment from input, that is local/utc/zone equivalent to model.
    function cloneWithOffset(input, model) {
        var res, diff;
        if (model._isUTC) {
            res = model.clone();
            diff =
                (isMoment(input) || isDate(input)
                    ? input.valueOf()
                    : createLocal(input).valueOf()) - res.valueOf();
            // Use low-level api, because this fn is low-level api.
            res._d.setTime(res._d.valueOf() + diff);
            hooks.updateOffset(res, false);
            return res;
        } else {
            return createLocal(input).local();
        }
    }

    function getDateOffset(m) {
        // On Firefox.24 Date#getTimezoneOffset returns a floating point.
        // https://github.com/moment/moment/pull/1871
        return -Math.round(m._d.getTimezoneOffset());
    }

    // HOOKS

    // This function will be called whenever a moment is mutated.
    // It is intended to keep the offset in sync with the timezone.
    hooks.updateOffset = function () {};

    // MOMENTS

    // keepLocalTime = true means only change the timezone, without
    // affecting the local hour. So 5:31:26 +0300 --[utcOffset(2, true)]-->
    // 5:31:26 +0200 It is possible that 5:31:26 doesn't exist with offset
    // +0200, so we adjust the time as needed, to be valid.
    //
    // Keeping the time actually adds/subtracts (one hour)
    // from the actual represented time. That is why we call updateOffset
    // a second time. In case it wants us to change the offset again
    // _changeInProgress == true case, then we have to adjust, because
    // there is no such time in the given timezone.
    function getSetOffset(input, keepLocalTime, keepMinutes) {
        var offset = this._offset || 0,
            localAdjust;
        if (!this.isValid()) {
            return input != null ? this : NaN;
        }
        if (input != null) {
            if (typeof input === 'string') {
                input = offsetFromString(matchShortOffset, input);
                if (input === null) {
                    return this;
                }
            } else if (Math.abs(input) < 16 && !keepMinutes) {
                input = input * 60;
            }
            if (!this._isUTC && keepLocalTime) {
                localAdjust = getDateOffset(this);
            }
            this._offset = input;
            this._isUTC = true;
            if (localAdjust != null) {
                this.add(localAdjust, 'm');
            }
            if (offset !== input) {
                if (!keepLocalTime || this._changeInProgress) {
                    addSubtract(
                        this,
                        createDuration(input - offset, 'm'),
                        1,
                        false
                    );
                } else if (!this._changeInProgress) {
                    this._changeInProgress = true;
                    hooks.updateOffset(this, true);
                    this._changeInProgress = null;
                }
            }
            return this;
        } else {
            return this._isUTC ? offset : getDateOffset(this);
        }
    }

    function getSetZone(input, keepLocalTime) {
        if (input != null) {
            if (typeof input !== 'string') {
                input = -input;
            }

            this.utcOffset(input, keepLocalTime);

            return this;
        } else {
            return -this.utcOffset();
        }
    }

    function setOffsetToUTC(keepLocalTime) {
        return this.utcOffset(0, keepLocalTime);
    }

    function setOffsetToLocal(keepLocalTime) {
        if (this._isUTC) {
            this.utcOffset(0, keepLocalTime);
            this._isUTC = false;

            if (keepLocalTime) {
                this.subtract(getDateOffset(this), 'm');
            }
        }
        return this;
    }

    function setOffsetToParsedOffset() {
        if (this._tzm != null) {
            this.utcOffset(this._tzm, false, true);
        } else if (typeof this._i === 'string') {
            var tZone = offsetFromString(matchOffset, this._i);
            if (tZone != null) {
                this.utcOffset(tZone);
            } else {
                this.utcOffset(0, true);
            }
        }
        return this;
    }

    function hasAlignedHourOffset(input) {
        if (!this.isValid()) {
            return false;
        }
        input = input ? createLocal(input).utcOffset() : 0;

        return (this.utcOffset() - input) % 60 === 0;
    }

    function isDaylightSavingTime() {
        return (
            this.utcOffset() > this.clone().month(0).utcOffset() ||
            this.utcOffset() > this.clone().month(5).utcOffset()
        );
    }

    function isDaylightSavingTimeShifted() {
        if (!isUndefined(this._isDSTShifted)) {
            return this._isDSTShifted;
        }

        var c = {},
            other;

        copyConfig(c, this);
        c = prepareConfig(c);

        if (c._a) {
            other = c._isUTC ? createUTC(c._a) : createLocal(c._a);
            this._isDSTShifted =
                this.isValid() && compareArrays(c._a, other.toArray()) > 0;
        } else {
            this._isDSTShifted = false;
        }

        return this._isDSTShifted;
    }

    function isLocal() {
        return this.isValid() ? !this._isUTC : false;
    }

    function isUtcOffset() {
        return this.isValid() ? this._isUTC : false;
    }

    function isUtc() {
        return this.isValid() ? this._isUTC && this._offset === 0 : false;
    }

    // ASP.NET json date format regex
    var aspNetRegex = /^(-|\+)?(?:(\d*)[. ])?(\d+):(\d+)(?::(\d+)(\.\d*)?)?$/,
        // from http://docs.closure-library.googlecode.com/git/closure_goog_date_date.js.source.html
        // somewhat more in line with 4.4.3.2 2004 spec, but allows decimal anywhere
        // and further modified to allow for strings containing both week and day
        isoRegex = /^(-|\+)?P(?:([-+]?[0-9,.]*)Y)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)W)?(?:([-+]?[0-9,.]*)D)?(?:T(?:([-+]?[0-9,.]*)H)?(?:([-+]?[0-9,.]*)M)?(?:([-+]?[0-9,.]*)S)?)?$/;

    function createDuration(input, key) {
        var duration = input,
            // matching against regexp is expensive, do it on demand
            match = null,
            sign,
            ret,
            diffRes;

        if (isDuration(input)) {
            duration = {
                ms: input._milliseconds,
                d: input._days,
                M: input._months,
            };
        } else if (isNumber(input) || !isNaN(+input)) {
            duration = {};
            if (key) {
                duration[key] = +input;
            } else {
                duration.milliseconds = +input;
            }
        } else if ((match = aspNetRegex.exec(input))) {
            sign = match[1] === '-' ? -1 : 1;
            duration = {
                y: 0,
                d: toInt(match[DATE]) * sign,
                h: toInt(match[HOUR]) * sign,
                m: toInt(match[MINUTE]) * sign,
                s: toInt(match[SECOND]) * sign,
                ms: toInt(absRound(match[MILLISECOND] * 1000)) * sign, // the millisecond decimal point is included in the match
            };
        } else if ((match = isoRegex.exec(input))) {
            sign = match[1] === '-' ? -1 : 1;
            duration = {
                y: parseIso(match[2], sign),
                M: parseIso(match[3], sign),
                w: parseIso(match[4], sign),
                d: parseIso(match[5], sign),
                h: parseIso(match[6], sign),
                m: parseIso(match[7], sign),
                s: parseIso(match[8], sign),
            };
        } else if (duration == null) {
            // checks for null or undefined
            duration = {};
        } else if (
            typeof duration === 'object' &&
            ('from' in duration || 'to' in duration)
        ) {
            diffRes = momentsDifference(
                createLocal(duration.from),
                createLocal(duration.to)
            );

            duration = {};
            duration.ms = diffRes.milliseconds;
            duration.M = diffRes.months;
        }

        ret = new Duration(duration);

        if (isDuration(input) && hasOwnProp(input, '_locale')) {
            ret._locale = input._locale;
        }

        if (isDuration(input) && hasOwnProp(input, '_isValid')) {
            ret._isValid = input._isValid;
        }

        return ret;
    }

    createDuration.fn = Duration.prototype;
    createDuration.invalid = createInvalid$1;

    function parseIso(inp, sign) {
        // We'd normally use ~~inp for this, but unfortunately it also
        // converts floats to ints.
        // inp may be undefined, so careful calling replace on it.
        var res = inp && parseFloat(inp.replace(',', '.'));
        // apply sign while we're at it
        return (isNaN(res) ? 0 : res) * sign;
    }

    function positiveMomentsDifference(base, other) {
        var res = {};

        res.months =
            other.month() - base.month() + (other.year() - base.year()) * 12;
        if (base.clone().add(res.months, 'M').isAfter(other)) {
            --res.months;
        }

        res.milliseconds = +other - +base.clone().add(res.months, 'M');

        return res;
    }

    function momentsDifference(base, other) {
        var res;
        if (!(base.isValid() && other.isValid())) {
            return { milliseconds: 0, months: 0 };
        }

        other = cloneWithOffset(other, base);
        if (base.isBefore(other)) {
            res = positiveMomentsDifference(base, other);
        } else {
            res = positiveMomentsDifference(other, base);
            res.milliseconds = -res.milliseconds;
            res.months = -res.months;
        }

        return res;
    }

    // TODO: remove 'name' arg after deprecation is removed
    function createAdder(direction, name) {
        return function (val, period) {
            var dur, tmp;
            //invert the arguments, but complain about it
            if (period !== null && !isNaN(+period)) {
                deprecateSimple(
                    name,
                    'moment().' +
                        name +
                        '(period, number) is deprecated. Please use moment().' +
                        name +
                        '(number, period). ' +
                        'See http://momentjs.com/guides/#/warnings/add-inverted-param/ for more info.'
                );
                tmp = val;
                val = period;
                period = tmp;
            }

            dur = createDuration(val, period);
            addSubtract(this, dur, direction);
            return this;
        };
    }

    function addSubtract(mom, duration, isAdding, updateOffset) {
        var milliseconds = duration._milliseconds,
            days = absRound(duration._days),
            months = absRound(duration._months);

        if (!mom.isValid()) {
            // No op
            return;
        }

        updateOffset = updateOffset == null ? true : updateOffset;

        if (months) {
            setMonth(mom, get(mom, 'Month') + months * isAdding);
        }
        if (days) {
            set$1(mom, 'Date', get(mom, 'Date') + days * isAdding);
        }
        if (milliseconds) {
            mom._d.setTime(mom._d.valueOf() + milliseconds * isAdding);
        }
        if (updateOffset) {
            hooks.updateOffset(mom, days || months);
        }
    }

    var add = createAdder(1, 'add'),
        subtract = createAdder(-1, 'subtract');

    function isString(input) {
        return typeof input === 'string' || input instanceof String;
    }

    // type MomentInput = Moment | Date | string | number | (number | string)[] | MomentInputObject | void; // null | undefined
    function isMomentInput(input) {
        return (
            isMoment(input) ||
            isDate(input) ||
            isString(input) ||
            isNumber(input) ||
            isNumberOrStringArray(input) ||
            isMomentInputObject(input) ||
            input === null ||
            input === undefined
        );
    }

    function isMomentInputObject(input) {
        var objectTest = isObject(input) && !isObjectEmpty(input),
            propertyTest = false,
            properties = [
                'years',
                'year',
                'y',
                'months',
                'month',
                'M',
                'days',
                'day',
                'd',
                'dates',
                'date',
                'D',
                'hours',
                'hour',
                'h',
                'minutes',
                'minute',
                'm',
                'seconds',
                'second',
                's',
                'milliseconds',
                'millisecond',
                'ms',
            ],
            i,
            property;

        for (i = 0; i < properties.length; i += 1) {
            property = properties[i];
            propertyTest = propertyTest || hasOwnProp(input, property);
        }

        return objectTest && propertyTest;
    }

    function isNumberOrStringArray(input) {
        var arrayTest = isArray(input),
            dataTypeTest = false;
        if (arrayTest) {
            dataTypeTest =
                input.filter(function (item) {
                    return !isNumber(item) && isString(input);
                }).length === 0;
        }
        return arrayTest && dataTypeTest;
    }

    function isCalendarSpec(input) {
        var objectTest = isObject(input) && !isObjectEmpty(input),
            propertyTest = false,
            properties = [
                'sameDay',
                'nextDay',
                'lastDay',
                'nextWeek',
                'lastWeek',
                'sameElse',
            ],
            i,
            property;

        for (i = 0; i < properties.length; i += 1) {
            property = properties[i];
            propertyTest = propertyTest || hasOwnProp(input, property);
        }

        return objectTest && propertyTest;
    }

    function getCalendarFormat(myMoment, now) {
        var diff = myMoment.diff(now, 'days', true);
        return diff < -6
            ? 'sameElse'
            : diff < -1
            ? 'lastWeek'
            : diff < 0
            ? 'lastDay'
            : diff < 1
            ? 'sameDay'
            : diff < 2
            ? 'nextDay'
            : diff < 7
            ? 'nextWeek'
            : 'sameElse';
    }

    function calendar$1(time, formats) {
        // Support for single parameter, formats only overload to the calendar function
        if (arguments.length === 1) {
            if (!arguments[0]) {
                time = undefined;
                formats = undefined;
            } else if (isMomentInput(arguments[0])) {
                time = arguments[0];
                formats = undefined;
            } else if (isCalendarSpec(arguments[0])) {
                formats = arguments[0];
                time = undefined;
            }
        }
        // We want to compare the start of today, vs this.
        // Getting start-of-today depends on whether we're local/utc/offset or not.
        var now = time || createLocal(),
            sod = cloneWithOffset(now, this).startOf('day'),
            format = hooks.calendarFormat(this, sod) || 'sameElse',
            output =
                formats &&
                (isFunction(formats[format])
                    ? formats[format].call(this, now)
                    : formats[format]);

        return this.format(
            output || this.localeData().calendar(format, this, createLocal(now))
        );
    }

    function clone() {
        return new Moment(this);
    }

    function isAfter(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input);
        if (!(this.isValid() && localInput.isValid())) {
            return false;
        }
        units = normalizeUnits(units) || 'millisecond';
        if (units === 'millisecond') {
            return this.valueOf() > localInput.valueOf();
        } else {
            return localInput.valueOf() < this.clone().startOf(units).valueOf();
        }
    }

    function isBefore(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input);
        if (!(this.isValid() && localInput.isValid())) {
            return false;
        }
        units = normalizeUnits(units) || 'millisecond';
        if (units === 'millisecond') {
            return this.valueOf() < localInput.valueOf();
        } else {
            return this.clone().endOf(units).valueOf() < localInput.valueOf();
        }
    }

    function isBetween(from, to, units, inclusivity) {
        var localFrom = isMoment(from) ? from : createLocal(from),
            localTo = isMoment(to) ? to : createLocal(to);
        if (!(this.isValid() && localFrom.isValid() && localTo.isValid())) {
            return false;
        }
        inclusivity = inclusivity || '()';
        return (
            (inclusivity[0] === '('
                ? this.isAfter(localFrom, units)
                : !this.isBefore(localFrom, units)) &&
            (inclusivity[1] === ')'
                ? this.isBefore(localTo, units)
                : !this.isAfter(localTo, units))
        );
    }

    function isSame(input, units) {
        var localInput = isMoment(input) ? input : createLocal(input),
            inputMs;
        if (!(this.isValid() && localInput.isValid())) {
            return false;
        }
        units = normalizeUnits(units) || 'millisecond';
        if (units === 'millisecond') {
            return this.valueOf() === localInput.valueOf();
        } else {
            inputMs = localInput.valueOf();
            return (
                this.clone().startOf(units).valueOf() <= inputMs &&
                inputMs <= this.clone().endOf(units).valueOf()
            );
        }
    }

    function isSameOrAfter(input, units) {
        return this.isSame(input, units) || this.isAfter(input, units);
    }

    function isSameOrBefore(input, units) {
        return this.isSame(input, units) || this.isBefore(input, units);
    }

    function diff(input, units, asFloat) {
        var that, zoneDelta, output;

        if (!this.isValid()) {
            return NaN;
        }

        that = cloneWithOffset(input, this);

        if (!that.isValid()) {
            return NaN;
        }

        zoneDelta = (that.utcOffset() - this.utcOffset()) * 6e4;

        units = normalizeUnits(units);

        switch (units) {
            case 'year':
                output = monthDiff(this, that) / 12;
                break;
            case 'month':
                output = monthDiff(this, that);
                break;
            case 'quarter':
                output = monthDiff(this, that) / 3;
                break;
            case 'second':
                output = (this - that) / 1e3;
                break; // 1000
            case 'minute':
                output = (this - that) / 6e4;
                break; // 1000 * 60
            case 'hour':
                output = (this - that) / 36e5;
                break; // 1000 * 60 * 60
            case 'day':
                output = (this - that - zoneDelta) / 864e5;
                break; // 1000 * 60 * 60 * 24, negate dst
            case 'week':
                output = (this - that - zoneDelta) / 6048e5;
                break; // 1000 * 60 * 60 * 24 * 7, negate dst
            default:
                output = this - that;
        }

        return asFloat ? output : absFloor(output);
    }

    function monthDiff(a, b) {
        if (a.date() < b.date()) {
            // end-of-month calculations work correct when the start month has more
            // days than the end month.
            return -monthDiff(b, a);
        }
        // difference in months
        var wholeMonthDiff = (b.year() - a.year()) * 12 + (b.month() - a.month()),
            // b is in (anchor - 1 month, anchor + 1 month)
            anchor = a.clone().add(wholeMonthDiff, 'months'),
            anchor2,
            adjust;

        if (b - anchor < 0) {
            anchor2 = a.clone().add(wholeMonthDiff - 1, 'months');
            // linear across the month
            adjust = (b - anchor) / (anchor - anchor2);
        } else {
            anchor2 = a.clone().add(wholeMonthDiff + 1, 'months');
            // linear across the month
            adjust = (b - anchor) / (anchor2 - anchor);
        }

        //check for negative zero, return zero if negative zero
        return -(wholeMonthDiff + adjust) || 0;
    }

    hooks.defaultFormat = 'YYYY-MM-DDTHH:mm:ssZ';
    hooks.defaultFormatUtc = 'YYYY-MM-DDTHH:mm:ss[Z]';

    function toString() {
        return this.clone().locale('en').format('ddd MMM DD YYYY HH:mm:ss [GMT]ZZ');
    }

    function toISOString(keepOffset) {
        if (!this.isValid()) {
            return null;
        }
        var utc = keepOffset !== true,
            m = utc ? this.clone().utc() : this;
        if (m.year() < 0 || m.year() > 9999) {
            return formatMoment(
                m,
                utc
                    ? 'YYYYYY-MM-DD[T]HH:mm:ss.SSS[Z]'
                    : 'YYYYYY-MM-DD[T]HH:mm:ss.SSSZ'
            );
        }
        if (isFunction(Date.prototype.toISOString)) {
            // native implementation is ~50x faster, use it when we can
            if (utc) {
                return this.toDate().toISOString();
            } else {
                return new Date(this.valueOf() + this.utcOffset() * 60 * 1000)
                    .toISOString()
                    .replace('Z', formatMoment(m, 'Z'));
            }
        }
        return formatMoment(
            m,
            utc ? 'YYYY-MM-DD[T]HH:mm:ss.SSS[Z]' : 'YYYY-MM-DD[T]HH:mm:ss.SSSZ'
        );
    }

    /**
     * Return a human readable representation of a moment that can
     * also be evaluated to get a new moment which is the same
     *
     * @link https://nodejs.org/dist/latest/docs/api/util.html#util_custom_inspect_function_on_objects
     */
    function inspect() {
        if (!this.isValid()) {
            return 'moment.invalid(/* ' + this._i + ' */)';
        }
        var func = 'moment',
            zone = '',
            prefix,
            year,
            datetime,
            suffix;
        if (!this.isLocal()) {
            func = this.utcOffset() === 0 ? 'moment.utc' : 'moment.parseZone';
            zone = 'Z';
        }
        prefix = '[' + func + '("]';
        year = 0 <= this.year() && this.year() <= 9999 ? 'YYYY' : 'YYYYYY';
        datetime = '-MM-DD[T]HH:mm:ss.SSS';
        suffix = zone + '[")]';

        return this.format(prefix + year + datetime + suffix);
    }

    function format(inputString) {
        if (!inputString) {
            inputString = this.isUtc()
                ? hooks.defaultFormatUtc
                : hooks.defaultFormat;
        }
        var output = formatMoment(this, inputString);
        return this.localeData().postformat(output);
    }

    function from(time, withoutSuffix) {
        if (
            this.isValid() &&
            ((isMoment(time) && time.isValid()) || createLocal(time).isValid())
        ) {
            return createDuration({ to: this, from: time })
                .locale(this.locale())
                .humanize(!withoutSuffix);
        } else {
            return this.localeData().invalidDate();
        }
    }

    function fromNow(withoutSuffix) {
        return this.from(createLocal(), withoutSuffix);
    }

    function to(time, withoutSuffix) {
        if (
            this.isValid() &&
            ((isMoment(time) && time.isValid()) || createLocal(time).isValid())
        ) {
            return createDuration({ from: this, to: time })
                .locale(this.locale())
                .humanize(!withoutSuffix);
        } else {
            return this.localeData().invalidDate();
        }
    }

    function toNow(withoutSuffix) {
        return this.to(createLocal(), withoutSuffix);
    }

    // If passed a locale key, it will set the locale for this
    // instance.  Otherwise, it will return the locale configuration
    // variables for this instance.
    function locale(key) {
        var newLocaleData;

        if (key === undefined) {
            return this._locale._abbr;
        } else {
            newLocaleData = getLocale(key);
            if (newLocaleData != null) {
                this._locale = newLocaleData;
            }
            return this;
        }
    }

    var lang = deprecate(
        'moment().lang() is deprecated. Instead, use moment().localeData() to get the language configuration. Use moment().locale() to change languages.',
        function (key) {
            if (key === undefined) {
                return this.localeData();
            } else {
                return this.locale(key);
            }
        }
    );

    function localeData() {
        return this._locale;
    }

    var MS_PER_SECOND = 1000,
        MS_PER_MINUTE = 60 * MS_PER_SECOND,
        MS_PER_HOUR = 60 * MS_PER_MINUTE,
        MS_PER_400_YEARS = (365 * 400 + 97) * 24 * MS_PER_HOUR;

    // actual modulo - handles negative numbers (for dates before 1970):
    function mod$1(dividend, divisor) {
        return ((dividend % divisor) + divisor) % divisor;
    }

    function localStartOfDate(y, m, d) {
        // the date constructor remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
            // preserve leap years using a full 400 year cycle, then reset
            return new Date(y + 400, m, d) - MS_PER_400_YEARS;
        } else {
            return new Date(y, m, d).valueOf();
        }
    }

    function utcStartOfDate(y, m, d) {
        // Date.UTC remaps years 0-99 to 1900-1999
        if (y < 100 && y >= 0) {
            // preserve leap years using a full 400 year cycle, then reset
            return Date.UTC(y + 400, m, d) - MS_PER_400_YEARS;
        } else {
            return Date.UTC(y, m, d);
        }
    }

    function startOf(units) {
        var time, startOfDate;
        units = normalizeUnits(units);
        if (units === undefined || units === 'millisecond' || !this.isValid()) {
            return this;
        }

        startOfDate = this._isUTC ? utcStartOfDate : localStartOfDate;

        switch (units) {
            case 'year':
                time = startOfDate(this.year(), 0, 1);
                break;
            case 'quarter':
                time = startOfDate(
                    this.year(),
                    this.month() - (this.month() % 3),
                    1
                );
                break;
            case 'month':
                time = startOfDate(this.year(), this.month(), 1);
                break;
            case 'week':
                time = startOfDate(
                    this.year(),
                    this.month(),
                    this.date() - this.weekday()
                );
                break;
            case 'isoWeek':
                time = startOfDate(
                    this.year(),
                    this.month(),
                    this.date() - (this.isoWeekday() - 1)
                );
                break;
            case 'day':
            case 'date':
                time = startOfDate(this.year(), this.month(), this.date());
                break;
            case 'hour':
                time = this._d.valueOf();
                time -= mod$1(
                    time + (this._isUTC ? 0 : this.utcOffset() * MS_PER_MINUTE),
                    MS_PER_HOUR
                );
                break;
            case 'minute':
                time = this._d.valueOf();
                time -= mod$1(time, MS_PER_MINUTE);
                break;
            case 'second':
                time = this._d.valueOf();
                time -= mod$1(time, MS_PER_SECOND);
                break;
        }

        this._d.setTime(time);
        hooks.updateOffset(this, true);
        return this;
    }

    function endOf(units) {
        var time, startOfDate;
        units = normalizeUnits(units);
        if (units === undefined || units === 'millisecond' || !this.isValid()) {
            return this;
        }

        startOfDate = this._isUTC ? utcStartOfDate : localStartOfDate;

        switch (units) {
            case 'year':
                time = startOfDate(this.year() + 1, 0, 1) - 1;
                break;
            case 'quarter':
                time =
                    startOfDate(
                        this.year(),
                        this.month() - (this.month() % 3) + 3,
                        1
                    ) - 1;
                break;
            case 'month':
                time = startOfDate(this.year(), this.month() + 1, 1) - 1;
                break;
            case 'week':
                time =
                    startOfDate(
                        this.year(),
                        this.month(),
                        this.date() - this.weekday() + 7
                    ) - 1;
                break;
            case 'isoWeek':
                time =
                    startOfDate(
                        this.year(),
                        this.month(),
                        this.date() - (this.isoWeekday() - 1) + 7
                    ) - 1;
                break;
            case 'day':
            case 'date':
                time = startOfDate(this.year(), this.month(), this.date() + 1) - 1;
                break;
            case 'hour':
                time = this._d.valueOf();
                time +=
                    MS_PER_HOUR -
                    mod$1(
                        time + (this._isUTC ? 0 : this.utcOffset() * MS_PER_MINUTE),
                        MS_PER_HOUR
                    ) -
                    1;
                break;
            case 'minute':
                time = this._d.valueOf();
                time += MS_PER_MINUTE - mod$1(time, MS_PER_MINUTE) - 1;
                break;
            case 'second':
                time = this._d.valueOf();
                time += MS_PER_SECOND - mod$1(time, MS_PER_SECOND) - 1;
                break;
        }

        this._d.setTime(time);
        hooks.updateOffset(this, true);
        return this;
    }

    function valueOf() {
        return this._d.valueOf() - (this._offset || 0) * 60000;
    }

    function unix() {
        return Math.floor(this.valueOf() / 1000);
    }

    function toDate() {
        return new Date(this.valueOf());
    }

    function toArray() {
        var m = this;
        return [
            m.year(),
            m.month(),
            m.date(),
            m.hour(),
            m.minute(),
            m.second(),
            m.millisecond(),
        ];
    }

    function toObject() {
        var m = this;
        return {
            years: m.year(),
            months: m.month(),
            date: m.date(),
            hours: m.hours(),
            minutes: m.minutes(),
            seconds: m.seconds(),
            milliseconds: m.milliseconds(),
        };
    }

    function toJSON() {
        // new Date(NaN).toJSON() === null
        return this.isValid() ? this.toISOString() : null;
    }

    function isValid$2() {
        return isValid(this);
    }

    function parsingFlags() {
        return extend({}, getParsingFlags(this));
    }

    function invalidAt() {
        return getParsingFlags(this).overflow;
    }

    function creationData() {
        return {
            input: this._i,
            format: this._f,
            locale: this._locale,
            isUTC: this._isUTC,
            strict: this._strict,
        };
    }

    addFormatToken('N', 0, 0, 'eraAbbr');
    addFormatToken('NN', 0, 0, 'eraAbbr');
    addFormatToken('NNN', 0, 0, 'eraAbbr');
    addFormatToken('NNNN', 0, 0, 'eraName');
    addFormatToken('NNNNN', 0, 0, 'eraNarrow');

    addFormatToken('y', ['y', 1], 'yo', 'eraYear');
    addFormatToken('y', ['yy', 2], 0, 'eraYear');
    addFormatToken('y', ['yyy', 3], 0, 'eraYear');
    addFormatToken('y', ['yyyy', 4], 0, 'eraYear');

    addRegexToken('N', matchEraAbbr);
    addRegexToken('NN', matchEraAbbr);
    addRegexToken('NNN', matchEraAbbr);
    addRegexToken('NNNN', matchEraName);
    addRegexToken('NNNNN', matchEraNarrow);

    addParseToken(['N', 'NN', 'NNN', 'NNNN', 'NNNNN'], function (
        input,
        array,
        config,
        token
    ) {
        var era = config._locale.erasParse(input, token, config._strict);
        if (era) {
            getParsingFlags(config).era = era;
        } else {
            getParsingFlags(config).invalidEra = input;
        }
    });

    addRegexToken('y', matchUnsigned);
    addRegexToken('yy', matchUnsigned);
    addRegexToken('yyy', matchUnsigned);
    addRegexToken('yyyy', matchUnsigned);
    addRegexToken('yo', matchEraYearOrdinal);

    addParseToken(['y', 'yy', 'yyy', 'yyyy'], YEAR);
    addParseToken(['yo'], function (input, array, config, token) {
        var match;
        if (config._locale._eraYearOrdinalRegex) {
            match = input.match(config._locale._eraYearOrdinalRegex);
        }

        if (config._locale.eraYearOrdinalParse) {
            array[YEAR] = config._locale.eraYearOrdinalParse(input, match);
        } else {
            array[YEAR] = parseInt(input, 10);
        }
    });

    function localeEras(m, format) {
        var i,
            l,
            date,
            eras = this._eras || getLocale('en')._eras;
        for (i = 0, l = eras.length; i < l; ++i) {
            switch (typeof eras[i].since) {
                case 'string':
                    // truncate time
                    date = hooks(eras[i].since).startOf('day');
                    eras[i].since = date.valueOf();
                    break;
            }

            switch (typeof eras[i].until) {
                case 'undefined':
                    eras[i].until = +Infinity;
                    break;
                case 'string':
                    // truncate time
                    date = hooks(eras[i].until).startOf('day').valueOf();
                    eras[i].until = date.valueOf();
                    break;
            }
        }
        return eras;
    }

    function localeErasParse(eraName, format, strict) {
        var i,
            l,
            eras = this.eras(),
            name,
            abbr,
            narrow;
        eraName = eraName.toUpperCase();

        for (i = 0, l = eras.length; i < l; ++i) {
            name = eras[i].name.toUpperCase();
            abbr = eras[i].abbr.toUpperCase();
            narrow = eras[i].narrow.toUpperCase();

            if (strict) {
                switch (format) {
                    case 'N':
                    case 'NN':
                    case 'NNN':
                        if (abbr === eraName) {
                            return eras[i];
                        }
                        break;

                    case 'NNNN':
                        if (name === eraName) {
                            return eras[i];
                        }
                        break;

                    case 'NNNNN':
                        if (narrow === eraName) {
                            return eras[i];
                        }
                        break;
                }
            } else if ([name, abbr, narrow].indexOf(eraName) >= 0) {
                return eras[i];
            }
        }
    }

    function localeErasConvertYear(era, year) {
        var dir = era.since <= era.until ? +1 : -1;
        if (year === undefined) {
            return hooks(era.since).year();
        } else {
            return hooks(era.since).year() + (year - era.offset) * dir;
        }
    }

    function getEraName() {
        var i,
            l,
            val,
            eras = this.localeData().eras();
        for (i = 0, l = eras.length; i < l; ++i) {
            // truncate time
            val = this.clone().startOf('day').valueOf();

            if (eras[i].since <= val && val <= eras[i].until) {
                return eras[i].name;
            }
            if (eras[i].until <= val && val <= eras[i].since) {
                return eras[i].name;
            }
        }

        return '';
    }

    function getEraNarrow() {
        var i,
            l,
            val,
            eras = this.localeData().eras();
        for (i = 0, l = eras.length; i < l; ++i) {
            // truncate time
            val = this.clone().startOf('day').valueOf();

            if (eras[i].since <= val && val <= eras[i].until) {
                return eras[i].narrow;
            }
            if (eras[i].until <= val && val <= eras[i].since) {
                return eras[i].narrow;
            }
        }

        return '';
    }

    function getEraAbbr() {
        var i,
            l,
            val,
            eras = this.localeData().eras();
        for (i = 0, l = eras.length; i < l; ++i) {
            // truncate time
            val = this.clone().startOf('day').valueOf();

            if (eras[i].since <= val && val <= eras[i].until) {
                return eras[i].abbr;
            }
            if (eras[i].until <= val && val <= eras[i].since) {
                return eras[i].abbr;
            }
        }

        return '';
    }

    function getEraYear() {
        var i,
            l,
            dir,
            val,
            eras = this.localeData().eras();
        for (i = 0, l = eras.length; i < l; ++i) {
            dir = eras[i].since <= eras[i].until ? +1 : -1;

            // truncate time
            val = this.clone().startOf('day').valueOf();

            if (
                (eras[i].since <= val && val <= eras[i].until) ||
                (eras[i].until <= val && val <= eras[i].since)
            ) {
                return (
                    (this.year() - hooks(eras[i].since).year()) * dir +
                    eras[i].offset
                );
            }
        }

        return this.year();
    }

    function erasNameRegex(isStrict) {
        if (!hasOwnProp(this, '_erasNameRegex')) {
            computeErasParse.call(this);
        }
        return isStrict ? this._erasNameRegex : this._erasRegex;
    }

    function erasAbbrRegex(isStrict) {
        if (!hasOwnProp(this, '_erasAbbrRegex')) {
            computeErasParse.call(this);
        }
        return isStrict ? this._erasAbbrRegex : this._erasRegex;
    }

    function erasNarrowRegex(isStrict) {
        if (!hasOwnProp(this, '_erasNarrowRegex')) {
            computeErasParse.call(this);
        }
        return isStrict ? this._erasNarrowRegex : this._erasRegex;
    }

    function matchEraAbbr(isStrict, locale) {
        return locale.erasAbbrRegex(isStrict);
    }

    function matchEraName(isStrict, locale) {
        return locale.erasNameRegex(isStrict);
    }

    function matchEraNarrow(isStrict, locale) {
        return locale.erasNarrowRegex(isStrict);
    }

    function matchEraYearOrdinal(isStrict, locale) {
        return locale._eraYearOrdinalRegex || matchUnsigned;
    }

    function computeErasParse() {
        var abbrPieces = [],
            namePieces = [],
            narrowPieces = [],
            mixedPieces = [],
            i,
            l,
            eras = this.eras();

        for (i = 0, l = eras.length; i < l; ++i) {
            namePieces.push(regexEscape(eras[i].name));
            abbrPieces.push(regexEscape(eras[i].abbr));
            narrowPieces.push(regexEscape(eras[i].narrow));

            mixedPieces.push(regexEscape(eras[i].name));
            mixedPieces.push(regexEscape(eras[i].abbr));
            mixedPieces.push(regexEscape(eras[i].narrow));
        }

        this._erasRegex = new RegExp('^(' + mixedPieces.join('|') + ')', 'i');
        this._erasNameRegex = new RegExp('^(' + namePieces.join('|') + ')', 'i');
        this._erasAbbrRegex = new RegExp('^(' + abbrPieces.join('|') + ')', 'i');
        this._erasNarrowRegex = new RegExp(
            '^(' + narrowPieces.join('|') + ')',
            'i'
        );
    }

    // FORMATTING

    addFormatToken(0, ['gg', 2], 0, function () {
        return this.weekYear() % 100;
    });

    addFormatToken(0, ['GG', 2], 0, function () {
        return this.isoWeekYear() % 100;
    });

    function addWeekYearFormatToken(token, getter) {
        addFormatToken(0, [token, token.length], 0, getter);
    }

    addWeekYearFormatToken('gggg', 'weekYear');
    addWeekYearFormatToken('ggggg', 'weekYear');
    addWeekYearFormatToken('GGGG', 'isoWeekYear');
    addWeekYearFormatToken('GGGGG', 'isoWeekYear');

    // ALIASES

    addUnitAlias('weekYear', 'gg');
    addUnitAlias('isoWeekYear', 'GG');

    // PRIORITY

    addUnitPriority('weekYear', 1);
    addUnitPriority('isoWeekYear', 1);

    // PARSING

    addRegexToken('G', matchSigned);
    addRegexToken('g', matchSigned);
    addRegexToken('GG', match1to2, match2);
    addRegexToken('gg', match1to2, match2);
    addRegexToken('GGGG', match1to4, match4);
    addRegexToken('gggg', match1to4, match4);
    addRegexToken('GGGGG', match1to6, match6);
    addRegexToken('ggggg', match1to6, match6);

    addWeekParseToken(['gggg', 'ggggg', 'GGGG', 'GGGGG'], function (
        input,
        week,
        config,
        token
    ) {
        week[token.substr(0, 2)] = toInt(input);
    });

    addWeekParseToken(['gg', 'GG'], function (input, week, config, token) {
        week[token] = hooks.parseTwoDigitYear(input);
    });

    // MOMENTS

    function getSetWeekYear(input) {
        return getSetWeekYearHelper.call(
            this,
            input,
            this.week(),
            this.weekday(),
            this.localeData()._week.dow,
            this.localeData()._week.doy
        );
    }

    function getSetISOWeekYear(input) {
        return getSetWeekYearHelper.call(
            this,
            input,
            this.isoWeek(),
            this.isoWeekday(),
            1,
            4
        );
    }

    function getISOWeeksInYear() {
        return weeksInYear(this.year(), 1, 4);
    }

    function getISOWeeksInISOWeekYear() {
        return weeksInYear(this.isoWeekYear(), 1, 4);
    }

    function getWeeksInYear() {
        var weekInfo = this.localeData()._week;
        return weeksInYear(this.year(), weekInfo.dow, weekInfo.doy);
    }

    function getWeeksInWeekYear() {
        var weekInfo = this.localeData()._week;
        return weeksInYear(this.weekYear(), weekInfo.dow, weekInfo.doy);
    }

    function getSetWeekYearHelper(input, week, weekday, dow, doy) {
        var weeksTarget;
        if (input == null) {
            return weekOfYear(this, dow, doy).year;
        } else {
            weeksTarget = weeksInYear(input, dow, doy);
            if (week > weeksTarget) {
                week = weeksTarget;
            }
            return setWeekAll.call(this, input, week, weekday, dow, doy);
        }
    }

    function setWeekAll(weekYear, week, weekday, dow, doy) {
        var dayOfYearData = dayOfYearFromWeeks(weekYear, week, weekday, dow, doy),
            date = createUTCDate(dayOfYearData.year, 0, dayOfYearData.dayOfYear);

        this.year(date.getUTCFullYear());
        this.month(date.getUTCMonth());
        this.date(date.getUTCDate());
        return this;
    }

    // FORMATTING

    addFormatToken('Q', 0, 'Qo', 'quarter');

    // ALIASES

    addUnitAlias('quarter', 'Q');

    // PRIORITY

    addUnitPriority('quarter', 7);

    // PARSING

    addRegexToken('Q', match1);
    addParseToken('Q', function (input, array) {
        array[MONTH] = (toInt(input) - 1) * 3;
    });

    // MOMENTS

    function getSetQuarter(input) {
        return input == null
            ? Math.ceil((this.month() + 1) / 3)
            : this.month((input - 1) * 3 + (this.month() % 3));
    }

    // FORMATTING

    addFormatToken('D', ['DD', 2], 'Do', 'date');

    // ALIASES

    addUnitAlias('date', 'D');

    // PRIORITY
    addUnitPriority('date', 9);

    // PARSING

    addRegexToken('D', match1to2);
    addRegexToken('DD', match1to2, match2);
    addRegexToken('Do', function (isStrict, locale) {
        // TODO: Remove "ordinalParse" fallback in next major release.
        return isStrict
            ? locale._dayOfMonthOrdinalParse || locale._ordinalParse
            : locale._dayOfMonthOrdinalParseLenient;
    });

    addParseToken(['D', 'DD'], DATE);
    addParseToken('Do', function (input, array) {
        array[DATE] = toInt(input.match(match1to2)[0]);
    });

    // MOMENTS

    var getSetDayOfMonth = makeGetSet('Date', true);

    // FORMATTING

    addFormatToken('DDD', ['DDDD', 3], 'DDDo', 'dayOfYear');

    // ALIASES

    addUnitAlias('dayOfYear', 'DDD');

    // PRIORITY
    addUnitPriority('dayOfYear', 4);

    // PARSING

    addRegexToken('DDD', match1to3);
    addRegexToken('DDDD', match3);
    addParseToken(['DDD', 'DDDD'], function (input, array, config) {
        config._dayOfYear = toInt(input);
    });

    // HELPERS

    // MOMENTS

    function getSetDayOfYear(input) {
        var dayOfYear =
            Math.round(
                (this.clone().startOf('day') - this.clone().startOf('year')) / 864e5
            ) + 1;
        return input == null ? dayOfYear : this.add(input - dayOfYear, 'd');
    }

    // FORMATTING

    addFormatToken('m', ['mm', 2], 0, 'minute');

    // ALIASES

    addUnitAlias('minute', 'm');

    // PRIORITY

    addUnitPriority('minute', 14);

    // PARSING

    addRegexToken('m', match1to2);
    addRegexToken('mm', match1to2, match2);
    addParseToken(['m', 'mm'], MINUTE);

    // MOMENTS

    var getSetMinute = makeGetSet('Minutes', false);

    // FORMATTING

    addFormatToken('s', ['ss', 2], 0, 'second');

    // ALIASES

    addUnitAlias('second', 's');

    // PRIORITY

    addUnitPriority('second', 15);

    // PARSING

    addRegexToken('s', match1to2);
    addRegexToken('ss', match1to2, match2);
    addParseToken(['s', 'ss'], SECOND);

    // MOMENTS

    var getSetSecond = makeGetSet('Seconds', false);

    // FORMATTING

    addFormatToken('S', 0, 0, function () {
        return ~~(this.millisecond() / 100);
    });

    addFormatToken(0, ['SS', 2], 0, function () {
        return ~~(this.millisecond() / 10);
    });

    addFormatToken(0, ['SSS', 3], 0, 'millisecond');
    addFormatToken(0, ['SSSS', 4], 0, function () {
        return this.millisecond() * 10;
    });
    addFormatToken(0, ['SSSSS', 5], 0, function () {
        return this.millisecond() * 100;
    });
    addFormatToken(0, ['SSSSSS', 6], 0, function () {
        return this.millisecond() * 1000;
    });
    addFormatToken(0, ['SSSSSSS', 7], 0, function () {
        return this.millisecond() * 10000;
    });
    addFormatToken(0, ['SSSSSSSS', 8], 0, function () {
        return this.millisecond() * 100000;
    });
    addFormatToken(0, ['SSSSSSSSS', 9], 0, function () {
        return this.millisecond() * 1000000;
    });

    // ALIASES

    addUnitAlias('millisecond', 'ms');

    // PRIORITY

    addUnitPriority('millisecond', 16);

    // PARSING

    addRegexToken('S', match1to3, match1);
    addRegexToken('SS', match1to3, match2);
    addRegexToken('SSS', match1to3, match3);

    var token, getSetMillisecond;
    for (token = 'SSSS'; token.length <= 9; token += 'S') {
        addRegexToken(token, matchUnsigned);
    }

    function parseMs(input, array) {
        array[MILLISECOND] = toInt(('0.' + input) * 1000);
    }

    for (token = 'S'; token.length <= 9; token += 'S') {
        addParseToken(token, parseMs);
    }

    getSetMillisecond = makeGetSet('Milliseconds', false);

    // FORMATTING

    addFormatToken('z', 0, 0, 'zoneAbbr');
    addFormatToken('zz', 0, 0, 'zoneName');

    // MOMENTS

    function getZoneAbbr() {
        return this._isUTC ? 'UTC' : '';
    }

    function getZoneName() {
        return this._isUTC ? 'Coordinated Universal Time' : '';
    }

    var proto = Moment.prototype;

    proto.add = add;
    proto.calendar = calendar$1;
    proto.clone = clone;
    proto.diff = diff;
    proto.endOf = endOf;
    proto.format = format;
    proto.from = from;
    proto.fromNow = fromNow;
    proto.to = to;
    proto.toNow = toNow;
    proto.get = stringGet;
    proto.invalidAt = invalidAt;
    proto.isAfter = isAfter;
    proto.isBefore = isBefore;
    proto.isBetween = isBetween;
    proto.isSame = isSame;
    proto.isSameOrAfter = isSameOrAfter;
    proto.isSameOrBefore = isSameOrBefore;
    proto.isValid = isValid$2;
    proto.lang = lang;
    proto.locale = locale;
    proto.localeData = localeData;
    proto.max = prototypeMax;
    proto.min = prototypeMin;
    proto.parsingFlags = parsingFlags;
    proto.set = stringSet;
    proto.startOf = startOf;
    proto.subtract = subtract;
    proto.toArray = toArray;
    proto.toObject = toObject;
    proto.toDate = toDate;
    proto.toISOString = toISOString;
    proto.inspect = inspect;
    if (typeof Symbol !== 'undefined' && Symbol.for != null) {
        proto[Symbol.for('nodejs.util.inspect.custom')] = function () {
            return 'Moment<' + this.format() + '>';
        };
    }
    proto.toJSON = toJSON;
    proto.toString = toString;
    proto.unix = unix;
    proto.valueOf = valueOf;
    proto.creationData = creationData;
    proto.eraName = getEraName;
    proto.eraNarrow = getEraNarrow;
    proto.eraAbbr = getEraAbbr;
    proto.eraYear = getEraYear;
    proto.year = getSetYear;
    proto.isLeapYear = getIsLeapYear;
    proto.weekYear = getSetWeekYear;
    proto.isoWeekYear = getSetISOWeekYear;
    proto.quarter = proto.quarters = getSetQuarter;
    proto.month = getSetMonth;
    proto.daysInMonth = getDaysInMonth;
    proto.week = proto.weeks = getSetWeek;
    proto.isoWeek = proto.isoWeeks = getSetISOWeek;
    proto.weeksInYear = getWeeksInYear;
    proto.weeksInWeekYear = getWeeksInWeekYear;
    proto.isoWeeksInYear = getISOWeeksInYear;
    proto.isoWeeksInISOWeekYear = getISOWeeksInISOWeekYear;
    proto.date = getSetDayOfMonth;
    proto.day = proto.days = getSetDayOfWeek;
    proto.weekday = getSetLocaleDayOfWeek;
    proto.isoWeekday = getSetISODayOfWeek;
    proto.dayOfYear = getSetDayOfYear;
    proto.hour = proto.hours = getSetHour;
    proto.minute = proto.minutes = getSetMinute;
    proto.second = proto.seconds = getSetSecond;
    proto.millisecond = proto.milliseconds = getSetMillisecond;
    proto.utcOffset = getSetOffset;
    proto.utc = setOffsetToUTC;
    proto.local = setOffsetToLocal;
    proto.parseZone = setOffsetToParsedOffset;
    proto.hasAlignedHourOffset = hasAlignedHourOffset;
    proto.isDST = isDaylightSavingTime;
    proto.isLocal = isLocal;
    proto.isUtcOffset = isUtcOffset;
    proto.isUtc = isUtc;
    proto.isUTC = isUtc;
    proto.zoneAbbr = getZoneAbbr;
    proto.zoneName = getZoneName;
    proto.dates = deprecate(
        'dates accessor is deprecated. Use date instead.',
        getSetDayOfMonth
    );
    proto.months = deprecate(
        'months accessor is deprecated. Use month instead',
        getSetMonth
    );
    proto.years = deprecate(
        'years accessor is deprecated. Use year instead',
        getSetYear
    );
    proto.zone = deprecate(
        'moment().zone is deprecated, use moment().utcOffset instead. http://momentjs.com/guides/#/warnings/zone/',
        getSetZone
    );
    proto.isDSTShifted = deprecate(
        'isDSTShifted is deprecated. See http://momentjs.com/guides/#/warnings/dst-shifted/ for more information',
        isDaylightSavingTimeShifted
    );

    function createUnix(input) {
        return createLocal(input * 1000);
    }

    function createInZone() {
        return createLocal.apply(null, arguments).parseZone();
    }

    function preParsePostFormat(string) {
        return string;
    }

    var proto$1 = Locale.prototype;

    proto$1.calendar = calendar;
    proto$1.longDateFormat = longDateFormat;
    proto$1.invalidDate = invalidDate;
    proto$1.ordinal = ordinal;
    proto$1.preparse = preParsePostFormat;
    proto$1.postformat = preParsePostFormat;
    proto$1.relativeTime = relativeTime;
    proto$1.pastFuture = pastFuture;
    proto$1.set = set;
    proto$1.eras = localeEras;
    proto$1.erasParse = localeErasParse;
    proto$1.erasConvertYear = localeErasConvertYear;
    proto$1.erasAbbrRegex = erasAbbrRegex;
    proto$1.erasNameRegex = erasNameRegex;
    proto$1.erasNarrowRegex = erasNarrowRegex;

    proto$1.months = localeMonths;
    proto$1.monthsShort = localeMonthsShort;
    proto$1.monthsParse = localeMonthsParse;
    proto$1.monthsRegex = monthsRegex;
    proto$1.monthsShortRegex = monthsShortRegex;
    proto$1.week = localeWeek;
    proto$1.firstDayOfYear = localeFirstDayOfYear;
    proto$1.firstDayOfWeek = localeFirstDayOfWeek;

    proto$1.weekdays = localeWeekdays;
    proto$1.weekdaysMin = localeWeekdaysMin;
    proto$1.weekdaysShort = localeWeekdaysShort;
    proto$1.weekdaysParse = localeWeekdaysParse;

    proto$1.weekdaysRegex = weekdaysRegex;
    proto$1.weekdaysShortRegex = weekdaysShortRegex;
    proto$1.weekdaysMinRegex = weekdaysMinRegex;

    proto$1.isPM = localeIsPM;
    proto$1.meridiem = localeMeridiem;

    function get$1(format, index, field, setter) {
        var locale = getLocale(),
            utc = createUTC().set(setter, index);
        return locale[field](utc, format);
    }

    function listMonthsImpl(format, index, field) {
        if (isNumber(format)) {
            index = format;
            format = undefined;
        }

        format = format || '';

        if (index != null) {
            return get$1(format, index, field, 'month');
        }

        var i,
            out = [];
        for (i = 0; i < 12; i++) {
            out[i] = get$1(format, i, field, 'month');
        }
        return out;
    }

    // ()
    // (5)
    // (fmt, 5)
    // (fmt)
    // (true)
    // (true, 5)
    // (true, fmt, 5)
    // (true, fmt)
    function listWeekdaysImpl(localeSorted, format, index, field) {
        if (typeof localeSorted === 'boolean') {
            if (isNumber(format)) {
                index = format;
                format = undefined;
            }

            format = format || '';
        } else {
            format = localeSorted;
            index = format;
            localeSorted = false;

            if (isNumber(format)) {
                index = format;
                format = undefined;
            }

            format = format || '';
        }

        var locale = getLocale(),
            shift = localeSorted ? locale._week.dow : 0,
            i,
            out = [];

        if (index != null) {
            return get$1(format, (index + shift) % 7, field, 'day');
        }

        for (i = 0; i < 7; i++) {
            out[i] = get$1(format, (i + shift) % 7, field, 'day');
        }
        return out;
    }

    function listMonths(format, index) {
        return listMonthsImpl(format, index, 'months');
    }

    function listMonthsShort(format, index) {
        return listMonthsImpl(format, index, 'monthsShort');
    }

    function listWeekdays(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdays');
    }

    function listWeekdaysShort(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdaysShort');
    }

    function listWeekdaysMin(localeSorted, format, index) {
        return listWeekdaysImpl(localeSorted, format, index, 'weekdaysMin');
    }

    getSetGlobalLocale('en', {
        eras: [
            {
                since: '0001-01-01',
                until: +Infinity,
                offset: 1,
                name: 'Anno Domini',
                narrow: 'AD',
                abbr: 'AD',
            },
            {
                since: '0000-12-31',
                until: -Infinity,
                offset: 1,
                name: 'Before Christ',
                narrow: 'BC',
                abbr: 'BC',
            },
        ],
        dayOfMonthOrdinalParse: /\d{1,2}(th|st|nd|rd)/,
        ordinal: function (number) {
            var b = number % 10,
                output =
                    toInt((number % 100) / 10) === 1
                        ? 'th'
                        : b === 1
                        ? 'st'
                        : b === 2
                        ? 'nd'
                        : b === 3
                        ? 'rd'
                        : 'th';
            return number + output;
        },
    });

    // Side effect imports

    hooks.lang = deprecate(
        'moment.lang is deprecated. Use moment.locale instead.',
        getSetGlobalLocale
    );
    hooks.langData = deprecate(
        'moment.langData is deprecated. Use moment.localeData instead.',
        getLocale
    );

    var mathAbs = Math.abs;

    function abs() {
        var data = this._data;

        this._milliseconds = mathAbs(this._milliseconds);
        this._days = mathAbs(this._days);
        this._months = mathAbs(this._months);

        data.milliseconds = mathAbs(data.milliseconds);
        data.seconds = mathAbs(data.seconds);
        data.minutes = mathAbs(data.minutes);
        data.hours = mathAbs(data.hours);
        data.months = mathAbs(data.months);
        data.years = mathAbs(data.years);

        return this;
    }

    function addSubtract$1(duration, input, value, direction) {
        var other = createDuration(input, value);

        duration._milliseconds += direction * other._milliseconds;
        duration._days += direction * other._days;
        duration._months += direction * other._months;

        return duration._bubble();
    }

    // supports only 2.0-style add(1, 's') or add(duration)
    function add$1(input, value) {
        return addSubtract$1(this, input, value, 1);
    }

    // supports only 2.0-style subtract(1, 's') or subtract(duration)
    function subtract$1(input, value) {
        return addSubtract$1(this, input, value, -1);
    }

    function absCeil(number) {
        if (number < 0) {
            return Math.floor(number);
        } else {
            return Math.ceil(number);
        }
    }

    function bubble() {
        var milliseconds = this._milliseconds,
            days = this._days,
            months = this._months,
            data = this._data,
            seconds,
            minutes,
            hours,
            years,
            monthsFromDays;

        // if we have a mix of positive and negative values, bubble down first
        // check: https://github.com/moment/moment/issues/2166
        if (
            !(
                (milliseconds >= 0 && days >= 0 && months >= 0) ||
                (milliseconds <= 0 && days <= 0 && months <= 0)
            )
        ) {
            milliseconds += absCeil(monthsToDays(months) + days) * 864e5;
            days = 0;
            months = 0;
        }

        // The following code bubbles up values, see the tests for
        // examples of what that means.
        data.milliseconds = milliseconds % 1000;

        seconds = absFloor(milliseconds / 1000);
        data.seconds = seconds % 60;

        minutes = absFloor(seconds / 60);
        data.minutes = minutes % 60;

        hours = absFloor(minutes / 60);
        data.hours = hours % 24;

        days += absFloor(hours / 24);

        // convert days to months
        monthsFromDays = absFloor(daysToMonths(days));
        months += monthsFromDays;
        days -= absCeil(monthsToDays(monthsFromDays));

        // 12 months -> 1 year
        years = absFloor(months / 12);
        months %= 12;

        data.days = days;
        data.months = months;
        data.years = years;

        return this;
    }

    function daysToMonths(days) {
        // 400 years have 146097 days (taking into account leap year rules)
        // 400 years have 12 months === 4800
        return (days * 4800) / 146097;
    }

    function monthsToDays(months) {
        // the reverse of daysToMonths
        return (months * 146097) / 4800;
    }

    function as(units) {
        if (!this.isValid()) {
            return NaN;
        }
        var days,
            months,
            milliseconds = this._milliseconds;

        units = normalizeUnits(units);

        if (units === 'month' || units === 'quarter' || units === 'year') {
            days = this._days + milliseconds / 864e5;
            months = this._months + daysToMonths(days);
            switch (units) {
                case 'month':
                    return months;
                case 'quarter':
                    return months / 3;
                case 'year':
                    return months / 12;
            }
        } else {
            // handle milliseconds separately because of floating point math errors (issue #1867)
            days = this._days + Math.round(monthsToDays(this._months));
            switch (units) {
                case 'week':
                    return days / 7 + milliseconds / 6048e5;
                case 'day':
                    return days + milliseconds / 864e5;
                case 'hour':
                    return days * 24 + milliseconds / 36e5;
                case 'minute':
                    return days * 1440 + milliseconds / 6e4;
                case 'second':
                    return days * 86400 + milliseconds / 1000;
                // Math.floor prevents floating point math errors here
                case 'millisecond':
                    return Math.floor(days * 864e5) + milliseconds;
                default:
                    throw new Error('Unknown unit ' + units);
            }
        }
    }

    // TODO: Use this.as('ms')?
    function valueOf$1() {
        if (!this.isValid()) {
            return NaN;
        }
        return (
            this._milliseconds +
            this._days * 864e5 +
            (this._months % 12) * 2592e6 +
            toInt(this._months / 12) * 31536e6
        );
    }

    function makeAs(alias) {
        return function () {
            return this.as(alias);
        };
    }

    var asMilliseconds = makeAs('ms'),
        asSeconds = makeAs('s'),
        asMinutes = makeAs('m'),
        asHours = makeAs('h'),
        asDays = makeAs('d'),
        asWeeks = makeAs('w'),
        asMonths = makeAs('M'),
        asQuarters = makeAs('Q'),
        asYears = makeAs('y');

    function clone$1() {
        return createDuration(this);
    }

    function get$2(units) {
        units = normalizeUnits(units);
        return this.isValid() ? this[units + 's']() : NaN;
    }

    function makeGetter(name) {
        return function () {
            return this.isValid() ? this._data[name] : NaN;
        };
    }

    var milliseconds = makeGetter('milliseconds'),
        seconds = makeGetter('seconds'),
        minutes = makeGetter('minutes'),
        hours = makeGetter('hours'),
        days = makeGetter('days'),
        months = makeGetter('months'),
        years = makeGetter('years');

    function weeks() {
        return absFloor(this.days() / 7);
    }

    var round = Math.round,
        thresholds = {
            ss: 44, // a few seconds to seconds
            s: 45, // seconds to minute
            m: 45, // minutes to hour
            h: 22, // hours to day
            d: 26, // days to month/week
            w: null, // weeks to month
            M: 11, // months to year
        };

    // helper function for moment.fn.from, moment.fn.fromNow, and moment.duration.fn.humanize
    function substituteTimeAgo(string, number, withoutSuffix, isFuture, locale) {
        return locale.relativeTime(number || 1, !!withoutSuffix, string, isFuture);
    }

    function relativeTime$1(posNegDuration, withoutSuffix, thresholds, locale) {
        var duration = createDuration(posNegDuration).abs(),
            seconds = round(duration.as('s')),
            minutes = round(duration.as('m')),
            hours = round(duration.as('h')),
            days = round(duration.as('d')),
            months = round(duration.as('M')),
            weeks = round(duration.as('w')),
            years = round(duration.as('y')),
            a =
                (seconds <= thresholds.ss && ['s', seconds]) ||
                (seconds < thresholds.s && ['ss', seconds]) ||
                (minutes <= 1 && ['m']) ||
                (minutes < thresholds.m && ['mm', minutes]) ||
                (hours <= 1 && ['h']) ||
                (hours < thresholds.h && ['hh', hours]) ||
                (days <= 1 && ['d']) ||
                (days < thresholds.d && ['dd', days]);

        if (thresholds.w != null) {
            a =
                a ||
                (weeks <= 1 && ['w']) ||
                (weeks < thresholds.w && ['ww', weeks]);
        }
        a = a ||
            (months <= 1 && ['M']) ||
            (months < thresholds.M && ['MM', months]) ||
            (years <= 1 && ['y']) || ['yy', years];

        a[2] = withoutSuffix;
        a[3] = +posNegDuration > 0;
        a[4] = locale;
        return substituteTimeAgo.apply(null, a);
    }

    // This function allows you to set the rounding function for relative time strings
    function getSetRelativeTimeRounding(roundingFunction) {
        if (roundingFunction === undefined) {
            return round;
        }
        if (typeof roundingFunction === 'function') {
            round = roundingFunction;
            return true;
        }
        return false;
    }

    // This function allows you to set a threshold for relative time strings
    function getSetRelativeTimeThreshold(threshold, limit) {
        if (thresholds[threshold] === undefined) {
            return false;
        }
        if (limit === undefined) {
            return thresholds[threshold];
        }
        thresholds[threshold] = limit;
        if (threshold === 's') {
            thresholds.ss = limit - 1;
        }
        return true;
    }

    function humanize(argWithSuffix, argThresholds) {
        if (!this.isValid()) {
            return this.localeData().invalidDate();
        }

        var withSuffix = false,
            th = thresholds,
            locale,
            output;

        if (typeof argWithSuffix === 'object') {
            argThresholds = argWithSuffix;
            argWithSuffix = false;
        }
        if (typeof argWithSuffix === 'boolean') {
            withSuffix = argWithSuffix;
        }
        if (typeof argThresholds === 'object') {
            th = Object.assign({}, thresholds, argThresholds);
            if (argThresholds.s != null && argThresholds.ss == null) {
                th.ss = argThresholds.s - 1;
            }
        }

        locale = this.localeData();
        output = relativeTime$1(this, !withSuffix, th, locale);

        if (withSuffix) {
            output = locale.pastFuture(+this, output);
        }

        return locale.postformat(output);
    }

    var abs$1 = Math.abs;

    function sign(x) {
        return (x > 0) - (x < 0) || +x;
    }

    function toISOString$1() {
        // for ISO strings we do not use the normal bubbling rules:
        //  * milliseconds bubble up until they become hours
        //  * days do not bubble at all
        //  * months bubble up until they become years
        // This is because there is no context-free conversion between hours and days
        // (think of clock changes)
        // and also not between days and months (28-31 days per month)
        if (!this.isValid()) {
            return this.localeData().invalidDate();
        }

        var seconds = abs$1(this._milliseconds) / 1000,
            days = abs$1(this._days),
            months = abs$1(this._months),
            minutes,
            hours,
            years,
            s,
            total = this.asSeconds(),
            totalSign,
            ymSign,
            daysSign,
            hmsSign;

        if (!total) {
            // this is the same as C#'s (Noda) and python (isodate)...
            // but not other JS (goog.date)
            return 'P0D';
        }

        // 3600 seconds -> 60 minutes -> 1 hour
        minutes = absFloor(seconds / 60);
        hours = absFloor(minutes / 60);
        seconds %= 60;
        minutes %= 60;

        // 12 months -> 1 year
        years = absFloor(months / 12);
        months %= 12;

        // inspired by https://github.com/dordille/moment-isoduration/blob/master/moment.isoduration.js
        s = seconds ? seconds.toFixed(3).replace(/\.?0+$/, '') : '';

        totalSign = total < 0 ? '-' : '';
        ymSign = sign(this._months) !== sign(total) ? '-' : '';
        daysSign = sign(this._days) !== sign(total) ? '-' : '';
        hmsSign = sign(this._milliseconds) !== sign(total) ? '-' : '';

        return (
            totalSign +
            'P' +
            (years ? ymSign + years + 'Y' : '') +
            (months ? ymSign + months + 'M' : '') +
            (days ? daysSign + days + 'D' : '') +
            (hours || minutes || seconds ? 'T' : '') +
            (hours ? hmsSign + hours + 'H' : '') +
            (minutes ? hmsSign + minutes + 'M' : '') +
            (seconds ? hmsSign + s + 'S' : '')
        );
    }

    var proto$2 = Duration.prototype;

    proto$2.isValid = isValid$1;
    proto$2.abs = abs;
    proto$2.add = add$1;
    proto$2.subtract = subtract$1;
    proto$2.as = as;
    proto$2.asMilliseconds = asMilliseconds;
    proto$2.asSeconds = asSeconds;
    proto$2.asMinutes = asMinutes;
    proto$2.asHours = asHours;
    proto$2.asDays = asDays;
    proto$2.asWeeks = asWeeks;
    proto$2.asMonths = asMonths;
    proto$2.asQuarters = asQuarters;
    proto$2.asYears = asYears;
    proto$2.valueOf = valueOf$1;
    proto$2._bubble = bubble;
    proto$2.clone = clone$1;
    proto$2.get = get$2;
    proto$2.milliseconds = milliseconds;
    proto$2.seconds = seconds;
    proto$2.minutes = minutes;
    proto$2.hours = hours;
    proto$2.days = days;
    proto$2.weeks = weeks;
    proto$2.months = months;
    proto$2.years = years;
    proto$2.humanize = humanize;
    proto$2.toISOString = toISOString$1;
    proto$2.toString = toISOString$1;
    proto$2.toJSON = toISOString$1;
    proto$2.locale = locale;
    proto$2.localeData = localeData;

    proto$2.toIsoString = deprecate(
        'toIsoString() is deprecated. Please use toISOString() instead (notice the capitals)',
        toISOString$1
    );
    proto$2.lang = lang;

    // FORMATTING

    addFormatToken('X', 0, 0, 'unix');
    addFormatToken('x', 0, 0, 'valueOf');

    // PARSING

    addRegexToken('x', matchSigned);
    addRegexToken('X', matchTimestamp);
    addParseToken('X', function (input, array, config) {
        config._d = new Date(parseFloat(input) * 1000);
    });
    addParseToken('x', function (input, array, config) {
        config._d = new Date(toInt(input));
    });

    //! moment.js

    hooks.version = '2.29.1';

    setHookCallback(createLocal);

    hooks.fn = proto;
    hooks.min = min;
    hooks.max = max;
    hooks.now = now;
    hooks.utc = createUTC;
    hooks.unix = createUnix;
    hooks.months = listMonths;
    hooks.isDate = isDate;
    hooks.locale = getSetGlobalLocale;
    hooks.invalid = createInvalid;
    hooks.duration = createDuration;
    hooks.isMoment = isMoment;
    hooks.weekdays = listWeekdays;
    hooks.parseZone = createInZone;
    hooks.localeData = getLocale;
    hooks.isDuration = isDuration;
    hooks.monthsShort = listMonthsShort;
    hooks.weekdaysMin = listWeekdaysMin;
    hooks.defineLocale = defineLocale;
    hooks.updateLocale = updateLocale;
    hooks.locales = listLocales;
    hooks.weekdaysShort = listWeekdaysShort;
    hooks.normalizeUnits = normalizeUnits;
    hooks.relativeTimeRounding = getSetRelativeTimeRounding;
    hooks.relativeTimeThreshold = getSetRelativeTimeThreshold;
    hooks.calendarFormat = getCalendarFormat;
    hooks.prototype = proto;

    // currently HTML5 input type only supports 24-hour formats
    hooks.HTML5_FMT = {
        DATETIME_LOCAL: 'YYYY-MM-DDTHH:mm', // <input type="datetime-local" />
        DATETIME_LOCAL_SECONDS: 'YYYY-MM-DDTHH:mm:ss', // <input type="datetime-local" step="1" />
        DATETIME_LOCAL_MS: 'YYYY-MM-DDTHH:mm:ss.SSS', // <input type="datetime-local" step="0.001" />
        DATE: 'YYYY-MM-DD', // <input type="date" />
        TIME: 'HH:mm', // <input type="time" />
        TIME_SECONDS: 'HH:mm:ss', // <input type="time" step="1" />
        TIME_MS: 'HH:mm:ss.SSS', // <input type="time" step="0.001" />
        WEEK: 'GGGG-[W]WW', // <input type="week" />
        MONTH: 'YYYY-MM', // <input type="month" />
    };

    return hooks;

})));
//! moment-timezone.js
//! version : 0.5.31
//! Copyright (c) JS Foundation and other contributors
//! license : MIT
//! github.com/moment/moment-timezone

(function (root, factory) {
	"use strict";

	/*global define*/
	if (typeof module === 'object' && module.exports) {
		module.exports = factory(require('moment')); // Node
	} else if (typeof define === 'function' && define.amd) {
		define(['moment'], factory);                 // AMD
	} else {
		factory(root.moment);                        // Browser
	}
}(this, function (moment) {
	"use strict";

	// Resolves es6 module loading issue
	if (moment.version === undefined && moment.default) {
		moment = moment.default;
	}

	// Do not load moment-timezone a second time.
	// if (moment.tz !== undefined) {
	// 	logError('Moment Timezone ' + moment.tz.version + ' was already loaded ' + (moment.tz.dataVersion ? 'with data from ' : 'without any data') + moment.tz.dataVersion);
	// 	return moment;
	// }

	var VERSION = "0.5.31",
		zones = {},
		links = {},
		countries = {},
		names = {},
		guesses = {},
		cachedGuess;

	if (!moment || typeof moment.version !== 'string') {
		logError('Moment Timezone requires Moment.js. See https://momentjs.com/timezone/docs/#/use-it/browser/');
	}

	var momentVersion = moment.version.split('.'),
		major = +momentVersion[0],
		minor = +momentVersion[1];

	// Moment.js version check
	if (major < 2 || (major === 2 && minor < 6)) {
		logError('Moment Timezone requires Moment.js >= 2.6.0. You are using Moment.js ' + moment.version + '. See momentjs.com');
	}

	/************************************
		Unpacking
	************************************/

	function charCodeToInt(charCode) {
		if (charCode > 96) {
			return charCode - 87;
		} else if (charCode > 64) {
			return charCode - 29;
		}
		return charCode - 48;
	}

	function unpackBase60(string) {
		var i = 0,
			parts = string.split('.'),
			whole = parts[0],
			fractional = parts[1] || '',
			multiplier = 1,
			num,
			out = 0,
			sign = 1;

		// handle negative numbers
		if (string.charCodeAt(0) === 45) {
			i = 1;
			sign = -1;
		}

		// handle digits before the decimal
		for (i; i < whole.length; i++) {
			num = charCodeToInt(whole.charCodeAt(i));
			out = 60 * out + num;
		}

		// handle digits after the decimal
		for (i = 0; i < fractional.length; i++) {
			multiplier = multiplier / 60;
			num = charCodeToInt(fractional.charCodeAt(i));
			out += num * multiplier;
		}

		return out * sign;
	}

	function arrayToInt (array) {
		for (var i = 0; i < array.length; i++) {
			array[i] = unpackBase60(array[i]);
		}
	}

	function intToUntil (array, length) {
		for (var i = 0; i < length; i++) {
			array[i] = Math.round((array[i - 1] || 0) + (array[i] * 60000)); // minutes to milliseconds
		}

		array[length - 1] = Infinity;
	}

	function mapIndices (source, indices) {
		var out = [], i;

		for (i = 0; i < indices.length; i++) {
			out[i] = source[indices[i]];
		}

		return out;
	}

	function unpack (string) {
		var data = string.split('|'),
			offsets = data[2].split(' '),
			indices = data[3].split(''),
			untils  = data[4].split(' ');

		arrayToInt(offsets);
		arrayToInt(indices);
		arrayToInt(untils);

		intToUntil(untils, indices.length);

		return {
			name       : data[0],
			abbrs      : mapIndices(data[1].split(' '), indices),
			offsets    : mapIndices(offsets, indices),
			untils     : untils,
			population : data[5] | 0
		};
	}

	/************************************
		Zone object
	************************************/

	function Zone (packedString) {
		if (packedString) {
			this._set(unpack(packedString));
		}
	}

	Zone.prototype = {
		_set : function (unpacked) {
			this.name       = unpacked.name;
			this.abbrs      = unpacked.abbrs;
			this.untils     = unpacked.untils;
			this.offsets    = unpacked.offsets;
			this.population = unpacked.population;
		},

		_index : function (timestamp) {
			var target = +timestamp,
				untils = this.untils,
				i;

			for (i = 0; i < untils.length; i++) {
				if (target < untils[i]) {
					return i;
				}
			}
		},

		countries : function () {
			var zone_name = this.name;
			return Object.keys(countries).filter(function (country_code) {
				return countries[country_code].zones.indexOf(zone_name) !== -1;
			});
		},

		parse : function (timestamp) {
			var target  = +timestamp,
				offsets = this.offsets,
				untils  = this.untils,
				max     = untils.length - 1,
				offset, offsetNext, offsetPrev, i;

			for (i = 0; i < max; i++) {
				offset     = offsets[i];
				offsetNext = offsets[i + 1];
				offsetPrev = offsets[i ? i - 1 : i];

				if (offset < offsetNext && tz.moveAmbiguousForward) {
					offset = offsetNext;
				} else if (offset > offsetPrev && tz.moveInvalidForward) {
					offset = offsetPrev;
				}

				if (target < untils[i] - (offset * 60000)) {
					return offsets[i];
				}
			}

			return offsets[max];
		},

		abbr : function (mom) {
			return this.abbrs[this._index(mom)];
		},

		offset : function (mom) {
			logError("zone.offset has been deprecated in favor of zone.utcOffset");
			return this.offsets[this._index(mom)];
		},

		utcOffset : function (mom) {
			return this.offsets[this._index(mom)];
		}
	};

	/************************************
		Country object
	************************************/

	function Country (country_name, zone_names) {
		this.name = country_name;
		this.zones = zone_names;
	}

	/************************************
		Current Timezone
	************************************/

	function OffsetAt(at) {
		var timeString = at.toTimeString();
		var abbr = timeString.match(/\([a-z ]+\)/i);
		if (abbr && abbr[0]) {
			// 17:56:31 GMT-0600 (CST)
			// 17:56:31 GMT-0600 (Central Standard Time)
			abbr = abbr[0].match(/[A-Z]/g);
			abbr = abbr ? abbr.join('') : undefined;
		} else {
			// 17:56:31 CST
			// 17:56:31 GMT+0800 (台北標準時間)
			abbr = timeString.match(/[A-Z]{3,5}/g);
			abbr = abbr ? abbr[0] : undefined;
		}

		if (abbr === 'GMT') {
			abbr = undefined;
		}

		this.at = +at;
		this.abbr = abbr;
		this.offset = at.getTimezoneOffset();
	}

	function ZoneScore(zone) {
		this.zone = zone;
		this.offsetScore = 0;
		this.abbrScore = 0;
	}

	ZoneScore.prototype.scoreOffsetAt = function (offsetAt) {
		this.offsetScore += Math.abs(this.zone.utcOffset(offsetAt.at) - offsetAt.offset);
		if (this.zone.abbr(offsetAt.at).replace(/[^A-Z]/g, '') !== offsetAt.abbr) {
			this.abbrScore++;
		}
	};

	function findChange(low, high) {
		var mid, diff;

		while ((diff = ((high.at - low.at) / 12e4 | 0) * 6e4)) {
			mid = new OffsetAt(new Date(low.at + diff));
			if (mid.offset === low.offset) {
				low = mid;
			} else {
				high = mid;
			}
		}

		return low;
	}

	function userOffsets() {
		var startYear = new Date().getFullYear() - 2,
			last = new OffsetAt(new Date(startYear, 0, 1)),
			offsets = [last],
			change, next, i;

		for (i = 1; i < 48; i++) {
			next = new OffsetAt(new Date(startYear, i, 1));
			if (next.offset !== last.offset) {
				change = findChange(last, next);
				offsets.push(change);
				offsets.push(new OffsetAt(new Date(change.at + 6e4)));
			}
			last = next;
		}

		for (i = 0; i < 4; i++) {
			offsets.push(new OffsetAt(new Date(startYear + i, 0, 1)));
			offsets.push(new OffsetAt(new Date(startYear + i, 6, 1)));
		}

		return offsets;
	}

	function sortZoneScores (a, b) {
		if (a.offsetScore !== b.offsetScore) {
			return a.offsetScore - b.offsetScore;
		}
		if (a.abbrScore !== b.abbrScore) {
			return a.abbrScore - b.abbrScore;
		}
		if (a.zone.population !== b.zone.population) {
			return b.zone.population - a.zone.population;
		}
		return b.zone.name.localeCompare(a.zone.name);
	}

	function addToGuesses (name, offsets) {
		var i, offset;
		arrayToInt(offsets);
		for (i = 0; i < offsets.length; i++) {
			offset = offsets[i];
			guesses[offset] = guesses[offset] || {};
			guesses[offset][name] = true;
		}
	}

	function guessesForUserOffsets (offsets) {
		var offsetsLength = offsets.length,
			filteredGuesses = {},
			out = [],
			i, j, guessesOffset;

		for (i = 0; i < offsetsLength; i++) {
			guessesOffset = guesses[offsets[i].offset] || {};
			for (j in guessesOffset) {
				if (guessesOffset.hasOwnProperty(j)) {
					filteredGuesses[j] = true;
				}
			}
		}

		for (i in filteredGuesses) {
			if (filteredGuesses.hasOwnProperty(i)) {
				out.push(names[i]);
			}
		}

		return out;
	}

	function rebuildGuess () {

		// use Intl API when available and returning valid time zone
		try {
			var intlName = Intl.DateTimeFormat().resolvedOptions().timeZone;
			if (intlName && intlName.length > 3) {
				var name = names[normalizeName(intlName)];
				if (name) {
					return name;
				}
				logError("Moment Timezone found " + intlName + " from the Intl api, but did not have that data loaded.");
			}
		} catch (e) {
			// Intl unavailable, fall back to manual guessing.
		}

		var offsets = userOffsets(),
			offsetsLength = offsets.length,
			guesses = guessesForUserOffsets(offsets),
			zoneScores = [],
			zoneScore, i, j;

		for (i = 0; i < guesses.length; i++) {
			zoneScore = new ZoneScore(getZone(guesses[i]), offsetsLength);
			for (j = 0; j < offsetsLength; j++) {
				zoneScore.scoreOffsetAt(offsets[j]);
			}
			zoneScores.push(zoneScore);
		}

		zoneScores.sort(sortZoneScores);

		return zoneScores.length > 0 ? zoneScores[0].zone.name : undefined;
	}

	function guess (ignoreCache) {
		if (!cachedGuess || ignoreCache) {
			cachedGuess = rebuildGuess();
		}
		return cachedGuess;
	}

	/************************************
		Global Methods
	************************************/

	function normalizeName (name) {
		return (name || '').toLowerCase().replace(/\//g, '_');
	}

	function addZone (packed) {
		var i, name, split, normalized;

		if (typeof packed === "string") {
			packed = [packed];
		}

		for (i = 0; i < packed.length; i++) {
			split = packed[i].split('|');
			name = split[0];
			normalized = normalizeName(name);
			zones[normalized] = packed[i];
			names[normalized] = name;
			addToGuesses(normalized, split[2].split(' '));
		}
	}

	function getZone (name, caller) {

		name = normalizeName(name);

		var zone = zones[name];
		var link;

		if (zone instanceof Zone) {
			return zone;
		}

		if (typeof zone === 'string') {
			zone = new Zone(zone);
			zones[name] = zone;
			return zone;
		}

		// Pass getZone to prevent recursion more than 1 level deep
		if (links[name] && caller !== getZone && (link = getZone(links[name], getZone))) {
			zone = zones[name] = new Zone();
			zone._set(link);
			zone.name = names[name];
			return zone;
		}

		return null;
	}

	function getNames () {
		var i, out = [];

		for (i in names) {
			if (names.hasOwnProperty(i) && (zones[i] || zones[links[i]]) && names[i]) {
				out.push(names[i]);
			}
		}

		return out.sort();
	}

	function getCountryNames () {
		return Object.keys(countries);
	}

	function addLink (aliases) {
		var i, alias, normal0, normal1;

		if (typeof aliases === "string") {
			aliases = [aliases];
		}

		for (i = 0; i < aliases.length; i++) {
			alias = aliases[i].split('|');

			normal0 = normalizeName(alias[0]);
			normal1 = normalizeName(alias[1]);

			links[normal0] = normal1;
			names[normal0] = alias[0];

			links[normal1] = normal0;
			names[normal1] = alias[1];
		}
	}

	function addCountries (data) {
		var i, country_code, country_zones, split;
		if (!data || !data.length) return;
		for (i = 0; i < data.length; i++) {
			split = data[i].split('|');
			country_code = split[0].toUpperCase();
			country_zones = split[1].split(' ');
			countries[country_code] = new Country(
				country_code,
				country_zones
			);
		}
	}

	function getCountry (name) {
		name = name.toUpperCase();
		return countries[name] || null;
	}

	function zonesForCountry(country, with_offset) {
		country = getCountry(country);

		if (!country) return null;

		var zones = country.zones.sort();

		if (with_offset) {
			return zones.map(function (zone_name) {
				var zone = getZone(zone_name);
				return {
					name: zone_name,
					offset: zone.utcOffset(new Date())
				};
			});
		}

		return zones;
	}

	function loadData (data) {
		addZone(data.zones);
		addLink(data.links);
		addCountries(data.countries);
		tz.dataVersion = data.version;
	}

	function zoneExists (name) {
		if (!zoneExists.didShowError) {
			zoneExists.didShowError = true;
				logError("moment.tz.zoneExists('" + name + "') has been deprecated in favor of !moment.tz.zone('" + name + "')");
		}
		return !!getZone(name);
	}

	function needsOffset (m) {
		var isUnixTimestamp = (m._f === 'X' || m._f === 'x');
		return !!(m._a && (m._tzm === undefined) && !isUnixTimestamp);
	}

	function logError (message) {
		if (typeof console !== 'undefined' && typeof console.error === 'function') {
			console.error(message);
		}
	}

	/************************************
		moment.tz namespace
	************************************/

	function tz (input) {
		var args = Array.prototype.slice.call(arguments, 0, -1),
			name = arguments[arguments.length - 1],
			zone = getZone(name),
			out  = moment.utc.apply(null, args);

		if (zone && !moment.isMoment(input) && needsOffset(out)) {
			out.add(zone.parse(out), 'minutes');
		}

		out.tz(name);

		return out;
	}

	tz.version      = VERSION;
	tz.dataVersion  = '';
	tz._zones       = zones;
	tz._links       = links;
	tz._names       = names;
	tz._countries	= countries;
	tz.add          = addZone;
	tz.link         = addLink;
	tz.load         = loadData;
	tz.zone         = getZone;
	tz.zoneExists   = zoneExists; // deprecated in 0.1.0
	tz.guess        = guess;
	tz.names        = getNames;
	tz.Zone         = Zone;
	tz.unpack       = unpack;
	tz.unpackBase60 = unpackBase60;
	tz.needsOffset  = needsOffset;
	tz.moveInvalidForward   = true;
	tz.moveAmbiguousForward = false;
	tz.countries    = getCountryNames;
	tz.zonesForCountry = zonesForCountry;

	/************************************
		Interface with Moment.js
	************************************/

	var fn = moment.fn;

	moment.tz = tz;

	moment.defaultZone = null;

	moment.updateOffset = function (mom, keepTime) {
		var zone = moment.defaultZone,
			offset;

		if (mom._z === undefined) {
			if (zone && needsOffset(mom) && !mom._isUTC) {
				mom._d = moment.utc(mom._a)._d;
				mom.utc().add(zone.parse(mom), 'minutes');
			}
			mom._z = zone;
		}
		if (mom._z) {
			offset = mom._z.utcOffset(mom);
			if (Math.abs(offset) < 16) {
				offset = offset / 60;
			}
			if (mom.utcOffset !== undefined) {
				var z = mom._z;
				mom.utcOffset(-offset, keepTime);
				mom._z = z;
			} else {
				mom.zone(offset, keepTime);
			}
		}
	};

	fn.tz = function (name, keepTime) {
		if (name) {
			if (typeof name !== 'string') {
				throw new Error('Time zone name must be a string, got ' + name + ' [' + typeof name + ']');
			}
			this._z = getZone(name);
			if (this._z) {
				moment.updateOffset(this, keepTime);
			} else {
				logError("Moment Timezone has no data for " + name + ". See http://momentjs.com/timezone/docs/#/data-loading/.");
			}
			return this;
		}
		if (this._z) { return this._z.name; }
	};

	function abbrWrap (old) {
		return function () {
			if (this._z) { return this._z.abbr(this); }
			return old.call(this);
		};
	}

	function resetZoneWrap (old) {
		return function () {
			this._z = null;
			return old.apply(this, arguments);
		};
	}

	function resetZoneWrap2 (old) {
		return function () {
			if (arguments.length > 0) this._z = null;
			return old.apply(this, arguments);
		};
	}

	fn.zoneName  = abbrWrap(fn.zoneName);
	fn.zoneAbbr  = abbrWrap(fn.zoneAbbr);
	fn.utc       = resetZoneWrap(fn.utc);
	fn.local     = resetZoneWrap(fn.local);
	fn.utcOffset = resetZoneWrap2(fn.utcOffset);

	moment.tz.setDefault = function(name) {
		if (major < 2 || (major === 2 && minor < 9)) {
			logError('Moment Timezone setDefault() requires Moment.js >= 2.9.0. You are using Moment.js ' + moment.version + '.');
		}
		moment.defaultZone = name ? getZone(name) : null;
		return moment;
	};

	// Cloning a moment should include the _z property.
	var momentProperties = moment.momentProperties;
	if (Object.prototype.toString.call(momentProperties) === '[object Array]') {
		// moment 2.8.1+
		momentProperties.push('_z');
		momentProperties.push('_a');
	} else if (momentProperties) {
		// moment 2.7.0
		momentProperties._z = null;
	}

	loadData({
		"version": "2020a",
		"zones": [
			"Africa/Abidjan|GMT|0|0||48e5",
			"Africa/Nairobi|EAT|-30|0||47e5",
			"Africa/Algiers|CET|-10|0||26e5",
			"Africa/Lagos|WAT|-10|0||17e6",
			"Africa/Maputo|CAT|-20|0||26e5",
			"Africa/Cairo|EET|-20|0||15e6",
			"Africa/Casablanca|+00 +01|0 -10|010101010101010101010101010101|1O9e0 uM0 e00 Dc0 11A0 s00 e00 IM0 WM0 mo0 gM0 LA0 WM0 jA0 e00 28M0 e00 2600 gM0 2600 e00 2600 gM0 2600 e00 28M0 e00 2600 gM0|32e5",
			"Europe/Paris|CET CEST|-10 -20|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|11e6",
			"Africa/Johannesburg|SAST|-20|0||84e5",
			"Africa/Khartoum|EAT CAT|-30 -20|01|1Usl0|51e5",
			"Africa/Sao_Tome|GMT WAT|0 -10|010|1UQN0 2q00|",
			"Africa/Windhoek|CAT WAT|-20 -10|0101010|1Oc00 11B0 1nX0 11B0 1nX0 11B0|32e4",
			"America/Adak|HST HDT|a0 90|01010101010101010101010|1O100 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|326",
			"America/Anchorage|AKST AKDT|90 80|01010101010101010101010|1O0X0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|30e4",
			"America/Santo_Domingo|AST|40|0||29e5",
			"America/Fortaleza|-03|30|0||34e5",
			"America/Asuncion|-03 -04|30 40|01010101010101010101010|1O6r0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0 17b0 1ip0 19X0 1fB0 19X0 1fB0 19X0 1fB0 19X0 1ip0 17b0 1ip0|28e5",
			"America/Panama|EST|50|0||15e5",
			"America/Mexico_City|CST CDT|60 50|01010101010101010101010|1Oc80 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|20e6",
			"America/Managua|CST|60|0||22e5",
			"America/La_Paz|-04|40|0||19e5",
			"America/Lima|-05|50|0||11e6",
			"America/Denver|MST MDT|70 60|01010101010101010101010|1O0V0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|26e5",
			"America/Campo_Grande|-03 -04|30 40|0101010101|1NTf0 1zd0 On0 1zd0 On0 1zd0 On0 1HB0 FX0|77e4",
			"America/Cancun|CST EST|60 50|01|1NKU0|63e4",
			"America/Caracas|-0430 -04|4u 40|01|1QMT0|29e5",
			"America/Chicago|CST CDT|60 50|01010101010101010101010|1O0U0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|92e5",
			"America/Chihuahua|MST MDT|70 60|01010101010101010101010|1Oc90 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0 14p0 1nX0 11B0 1nX0 11B0 1nX0 14p0 1lb0 14p0 1lb0|81e4",
			"America/Phoenix|MST|70|0||42e5",
			"America/Whitehorse|PST PDT MST|80 70 70|010101010102|1O0W0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0|23e3",
			"America/New_York|EST EDT|50 40|01010101010101010101010|1O0T0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|21e6",
			"America/Los_Angeles|PST PDT|80 70|01010101010101010101010|1O0W0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|15e6",
			"America/Fort_Nelson|PST MST|80 70|01|1O0W0|39e2",
			"America/Halifax|AST ADT|40 30|01010101010101010101010|1O0S0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|39e4",
			"America/Godthab|-03 -02|30 20|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|17e3",
			"America/Grand_Turk|EST EDT AST|50 40 40|0121010101010101010|1O0T0 1zb0 5Ip0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|37e2",
			"America/Havana|CST CDT|50 40|01010101010101010101010|1O0R0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Rc0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1zc0|21e5",
			"America/Metlakatla|PST AKST AKDT|80 90 80|01212120121212121212121|1PAa0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 uM0 jB0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|14e2",
			"America/Miquelon|-03 -02|30 20|01010101010101010101010|1O0R0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|61e2",
			"America/Montevideo|-02 -03|20 30|01|1O0Q0|17e5",
			"America/Noronha|-02|20|0||30e2",
			"America/Port-au-Prince|EST EDT|50 40|010101010101010101010|1O0T0 1zb0 3iN0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|23e5",
			"Antarctica/Palmer|-03 -04|30 40|010|1QSr0 Ap0|40",
			"America/Santiago|-03 -04|30 40|010101010101010101010|1QSr0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1zb0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 11B0 1nX0 11B0|62e5",
			"America/Sao_Paulo|-02 -03|20 30|0101010101|1NTe0 1zd0 On0 1zd0 On0 1zd0 On0 1HB0 FX0|20e6",
			"Atlantic/Azores|-01 +00|10 0|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|25e4",
			"America/St_Johns|NST NDT|3u 2u|01010101010101010101010|1O0Ru 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Rd0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0 Op0 1zb0|11e4",
			"Antarctica/Casey|+08 +11|-80 -b0|010|1RWg0 3m10|10",
			"Asia/Bangkok|+07|-70|0||15e6",
			"Asia/Vladivostok|+10|-a0|0||60e4",
			"Pacific/Bougainville|+11|-b0|0||18e4",
			"Asia/Tashkent|+05|-50|0||23e5",
			"Pacific/Auckland|NZDT NZST|-d0 -c0|01010101010101010101010|1ObO0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00|14e5",
			"Asia/Baghdad|+03|-30|0||66e5",
			"Antarctica/Troll|+00 +02|0 -20|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|40",
			"Asia/Dhaka|+06|-60|0||16e6",
			"Asia/Amman|EET EEST|-20 -30|01010101010101010101010|1O8m0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0|25e5",
			"Asia/Kamchatka|+12|-c0|0||18e4",
			"Asia/Baku|+04 +05|-40 -50|010|1O9c0 1o00|27e5",
			"Asia/Barnaul|+06 +07|-60 -70|01|1QyI0|",
			"Asia/Beirut|EET EEST|-20 -30|01010101010101010101010|1O9a0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0|22e5",
			"Asia/Kuala_Lumpur|+08|-80|0||71e5",
			"Asia/Kolkata|IST|-5u|0||15e6",
			"Asia/Chita|+08 +09|-80 -90|01|1QyG0|33e4",
			"Asia/Ulaanbaatar|+08 +09|-80 -90|01010|1O8G0 1cJ0 1cP0 1cJ0|12e5",
			"Asia/Shanghai|CST|-80|0||23e6",
			"Asia/Colombo|+0530|-5u|0||22e5",
			"Asia/Damascus|EET EEST|-20 -30|01010101010101010101010|1O8m0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 WN0 1qL0 WN0 1qL0 11B0 1nX0 11B0 1nX0 11B0 1qL0|26e5",
			"Asia/Yakutsk|+09|-90|0||28e4",
			"Asia/Dubai|+04|-40|0||39e5",
			"Asia/Famagusta|EET EEST +03|-20 -30 -30|0101201010101010101010|1O9d0 1o00 11A0 15U0 2Ks0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|",
			"Asia/Gaza|EET EEST|-20 -30|01010101010101010101010|1O8K0 1nz0 1220 1qL0 WN0 1qL0 WN0 1qL0 11c0 1oo0 11c0 1rc0 Wo0 1rc0 Wo0 1rc0 11c0 1oo0 11c0 1oo0 11c0 1oo0|18e5",
			"Asia/Hong_Kong|HKT|-80|0||73e5",
			"Asia/Hovd|+07 +08|-70 -80|01010|1O8H0 1cJ0 1cP0 1cJ0|81e3",
			"Europe/Istanbul|EET EEST +03|-20 -30 -30|01012|1O9d0 1tA0 U00 15w0|13e6",
			"Asia/Jakarta|WIB|-70|0||31e6",
			"Asia/Jayapura|WIT|-90|0||26e4",
			"Asia/Jerusalem|IST IDT|-20 -30|01010101010101010101010|1O8o0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0 10N0 1rz0 W10 1rz0 W10 1rz0 10N0 1oL0 10N0 1oL0|81e4",
			"Asia/Kabul|+0430|-4u|0||46e5",
			"Asia/Karachi|PKT|-50|0||24e6",
			"Asia/Kathmandu|+0545|-5J|0||12e5",
			"Asia/Magadan|+10 +11|-a0 -b0|01|1QJQ0|95e3",
			"Asia/Makassar|WITA|-80|0||15e5",
			"Asia/Manila|PST|-80|0||24e6",
			"Europe/Athens|EET EEST|-20 -30|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|35e5",
			"Asia/Novosibirsk|+06 +07|-60 -70|01|1Rmk0|15e5",
			"Asia/Pyongyang|KST KST|-90 -8u|010|1P4D0 6BA0|29e5",
			"Asia/Qyzylorda|+06 +05|-60 -50|01|1Xei0|73e4",
			"Asia/Rangoon|+0630|-6u|0||48e5",
			"Asia/Sakhalin|+10 +11|-a0 -b0|01|1QyE0|58e4",
			"Asia/Seoul|KST|-90|0||23e6",
			"Asia/Tehran|+0330 +0430|-3u -4u|01010101010101010101010|1O6ku 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0 1cp0 1dz0 1cp0 1dz0 1cp0 1dz0 1cN0 1dz0|14e6",
			"Asia/Tokyo|JST|-90|0||38e6",
			"Asia/Tomsk|+06 +07|-60 -70|01|1QXU0|10e5",
			"Europe/Lisbon|WET WEST|0 -10|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|27e5",
			"Atlantic/Cape_Verde|-01|10|0||50e4",
			"Australia/Sydney|AEDT AEST|-b0 -a0|01010101010101010101010|1ObQ0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0|40e5",
			"Australia/Adelaide|ACDT ACST|-au -9u|01010101010101010101010|1ObQu 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0|11e5",
			"Australia/Brisbane|AEST|-a0|0||20e5",
			"Australia/Darwin|ACST|-9u|0||12e4",
			"Australia/Eucla|+0845|-8J|0||368",
			"Australia/Lord_Howe|+11 +1030|-b0 -au|01010101010101010101010|1ObP0 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1fAu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1cLu 1cMu 1fzu 1cMu 1cLu 1cMu|347",
			"Australia/Perth|AWST|-80|0||18e5",
			"Pacific/Easter|-05 -06|50 60|010101010101010101010|1QSr0 Ap0 1Nb0 Ap0 1Nb0 Ap0 1zb0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1nX0 11B0 1qL0 11B0 1nX0 11B0|30e2",
			"Europe/Dublin|GMT IST|0 -10|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|12e5",
			"Etc/GMT-1|+01|-10|0||",
			"Pacific/Fakaofo|+13|-d0|0||483",
			"Pacific/Kiritimati|+14|-e0|0||51e2",
			"Etc/GMT-2|+02|-20|0||",
			"Pacific/Tahiti|-10|a0|0||18e4",
			"Pacific/Niue|-11|b0|0||12e2",
			"Etc/GMT+12|-12|c0|0||",
			"Pacific/Galapagos|-06|60|0||25e3",
			"Etc/GMT+7|-07|70|0||",
			"Pacific/Pitcairn|-08|80|0||56",
			"Pacific/Gambier|-09|90|0||125",
			"Etc/UTC|UTC|0|0||",
			"Europe/Ulyanovsk|+03 +04|-30 -40|01|1QyL0|13e5",
			"Europe/London|GMT BST|0 -10|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|10e6",
			"Europe/Chisinau|EET EEST|-20 -30|01010101010101010101010|1O9c0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|67e4",
			"Europe/Moscow|MSK|-30|0||16e6",
			"Europe/Saratov|+03 +04|-30 -40|01|1Sfz0|",
			"Europe/Volgograd|+03 +04|-30 -40|01|1WQL0|10e5",
			"Pacific/Honolulu|HST|a0|0||37e4",
			"MET|MET MEST|-10 -20|01010101010101010101010|1O9d0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00|",
			"Pacific/Chatham|+1345 +1245|-dJ -cJ|01010101010101010101010|1ObO0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00|600",
			"Pacific/Apia|+14 +13|-e0 -d0|01010101010101010101010|1ObO0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1cM0 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1fA0 1a00 1io0 1a00 1fA0 1a00|37e3",
			"Pacific/Fiji|+13 +12|-d0 -c0|01010101010101010101010|1NF20 1SM0 uM0 1VA0 s00 1VA0 s00 1VA0 s00 20o0 pc0 20o0 s00 20o0 pc0 20o0 pc0 20o0 pc0 20o0 pc0 20o0|88e4",
			"Pacific/Guam|ChST|-a0|0||17e4",
			"Pacific/Marquesas|-0930|9u|0||86e2",
			"Pacific/Pago_Pago|SST|b0|0||37e2",
			"Pacific/Norfolk|+1130 +11 +12|-bu -b0 -c0|012121212121212|1PoCu 9Jcu 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0|25e4",
			"Pacific/Tongatapu|+13 +14|-d0 -e0|010|1S4d0 s00|75e3"
		],
		"links": [
			"Africa/Abidjan|Africa/Accra",
			"Africa/Abidjan|Africa/Bamako",
			"Africa/Abidjan|Africa/Banjul",
			"Africa/Abidjan|Africa/Bissau",
			"Africa/Abidjan|Africa/Conakry",
			"Africa/Abidjan|Africa/Dakar",
			"Africa/Abidjan|Africa/Freetown",
			"Africa/Abidjan|Africa/Lome",
			"Africa/Abidjan|Africa/Monrovia",
			"Africa/Abidjan|Africa/Nouakchott",
			"Africa/Abidjan|Africa/Ouagadougou",
			"Africa/Abidjan|Africa/Timbuktu",
			"Africa/Abidjan|America/Danmarkshavn",
			"Africa/Abidjan|Atlantic/Reykjavik",
			"Africa/Abidjan|Atlantic/St_Helena",
			"Africa/Abidjan|Etc/GMT",
			"Africa/Abidjan|Etc/GMT+0",
			"Africa/Abidjan|Etc/GMT-0",
			"Africa/Abidjan|Etc/GMT0",
			"Africa/Abidjan|Etc/Greenwich",
			"Africa/Abidjan|GMT",
			"Africa/Abidjan|GMT+0",
			"Africa/Abidjan|GMT-0",
			"Africa/Abidjan|GMT0",
			"Africa/Abidjan|Greenwich",
			"Africa/Abidjan|Iceland",
			"Africa/Algiers|Africa/Tunis",
			"Africa/Cairo|Africa/Tripoli",
			"Africa/Cairo|Egypt",
			"Africa/Cairo|Europe/Kaliningrad",
			"Africa/Cairo|Libya",
			"Africa/Casablanca|Africa/El_Aaiun",
			"Africa/Johannesburg|Africa/Maseru",
			"Africa/Johannesburg|Africa/Mbabane",
			"Africa/Lagos|Africa/Bangui",
			"Africa/Lagos|Africa/Brazzaville",
			"Africa/Lagos|Africa/Douala",
			"Africa/Lagos|Africa/Kinshasa",
			"Africa/Lagos|Africa/Libreville",
			"Africa/Lagos|Africa/Luanda",
			"Africa/Lagos|Africa/Malabo",
			"Africa/Lagos|Africa/Ndjamena",
			"Africa/Lagos|Africa/Niamey",
			"Africa/Lagos|Africa/Porto-Novo",
			"Africa/Maputo|Africa/Blantyre",
			"Africa/Maputo|Africa/Bujumbura",
			"Africa/Maputo|Africa/Gaborone",
			"Africa/Maputo|Africa/Harare",
			"Africa/Maputo|Africa/Kigali",
			"Africa/Maputo|Africa/Lubumbashi",
			"Africa/Maputo|Africa/Lusaka",
			"Africa/Nairobi|Africa/Addis_Ababa",
			"Africa/Nairobi|Africa/Asmara",
			"Africa/Nairobi|Africa/Asmera",
			"Africa/Nairobi|Africa/Dar_es_Salaam",
			"Africa/Nairobi|Africa/Djibouti",
			"Africa/Nairobi|Africa/Juba",
			"Africa/Nairobi|Africa/Kampala",
			"Africa/Nairobi|Africa/Mogadishu",
			"Africa/Nairobi|Indian/Antananarivo",
			"Africa/Nairobi|Indian/Comoro",
			"Africa/Nairobi|Indian/Mayotte",
			"America/Adak|America/Atka",
			"America/Adak|US/Aleutian",
			"America/Anchorage|America/Juneau",
			"America/Anchorage|America/Nome",
			"America/Anchorage|America/Sitka",
			"America/Anchorage|America/Yakutat",
			"America/Anchorage|US/Alaska",
			"America/Campo_Grande|America/Cuiaba",
			"America/Chicago|America/Indiana/Knox",
			"America/Chicago|America/Indiana/Tell_City",
			"America/Chicago|America/Knox_IN",
			"America/Chicago|America/Matamoros",
			"America/Chicago|America/Menominee",
			"America/Chicago|America/North_Dakota/Beulah",
			"America/Chicago|America/North_Dakota/Center",
			"America/Chicago|America/North_Dakota/New_Salem",
			"America/Chicago|America/Rainy_River",
			"America/Chicago|America/Rankin_Inlet",
			"America/Chicago|America/Resolute",
			"America/Chicago|America/Winnipeg",
			"America/Chicago|CST6CDT",
			"America/Chicago|Canada/Central",
			"America/Chicago|US/Central",
			"America/Chicago|US/Indiana-Starke",
			"America/Chihuahua|America/Mazatlan",
			"America/Chihuahua|Mexico/BajaSur",
			"America/Denver|America/Boise",
			"America/Denver|America/Cambridge_Bay",
			"America/Denver|America/Edmonton",
			"America/Denver|America/Inuvik",
			"America/Denver|America/Ojinaga",
			"America/Denver|America/Shiprock",
			"America/Denver|America/Yellowknife",
			"America/Denver|Canada/Mountain",
			"America/Denver|MST7MDT",
			"America/Denver|Navajo",
			"America/Denver|US/Mountain",
			"America/Fortaleza|America/Araguaina",
			"America/Fortaleza|America/Argentina/Buenos_Aires",
			"America/Fortaleza|America/Argentina/Catamarca",
			"America/Fortaleza|America/Argentina/ComodRivadavia",
			"America/Fortaleza|America/Argentina/Cordoba",
			"America/Fortaleza|America/Argentina/Jujuy",
			"America/Fortaleza|America/Argentina/La_Rioja",
			"America/Fortaleza|America/Argentina/Mendoza",
			"America/Fortaleza|America/Argentina/Rio_Gallegos",
			"America/Fortaleza|America/Argentina/Salta",
			"America/Fortaleza|America/Argentina/San_Juan",
			"America/Fortaleza|America/Argentina/San_Luis",
			"America/Fortaleza|America/Argentina/Tucuman",
			"America/Fortaleza|America/Argentina/Ushuaia",
			"America/Fortaleza|America/Bahia",
			"America/Fortaleza|America/Belem",
			"America/Fortaleza|America/Buenos_Aires",
			"America/Fortaleza|America/Catamarca",
			"America/Fortaleza|America/Cayenne",
			"America/Fortaleza|America/Cordoba",
			"America/Fortaleza|America/Jujuy",
			"America/Fortaleza|America/Maceio",
			"America/Fortaleza|America/Mendoza",
			"America/Fortaleza|America/Paramaribo",
			"America/Fortaleza|America/Recife",
			"America/Fortaleza|America/Rosario",
			"America/Fortaleza|America/Santarem",
			"America/Fortaleza|Antarctica/Rothera",
			"America/Fortaleza|Atlantic/Stanley",
			"America/Fortaleza|Etc/GMT+3",
			"America/Godthab|America/Nuuk",
			"America/Halifax|America/Glace_Bay",
			"America/Halifax|America/Goose_Bay",
			"America/Halifax|America/Moncton",
			"America/Halifax|America/Thule",
			"America/Halifax|Atlantic/Bermuda",
			"America/Halifax|Canada/Atlantic",
			"America/Havana|Cuba",
			"America/La_Paz|America/Boa_Vista",
			"America/La_Paz|America/Guyana",
			"America/La_Paz|America/Manaus",
			"America/La_Paz|America/Porto_Velho",
			"America/La_Paz|Brazil/West",
			"America/La_Paz|Etc/GMT+4",
			"America/Lima|America/Bogota",
			"America/Lima|America/Eirunepe",
			"America/Lima|America/Guayaquil",
			"America/Lima|America/Porto_Acre",
			"America/Lima|America/Rio_Branco",
			"America/Lima|Brazil/Acre",
			"America/Lima|Etc/GMT+5",
			"America/Los_Angeles|America/Ensenada",
			"America/Los_Angeles|America/Santa_Isabel",
			"America/Los_Angeles|America/Tijuana",
			"America/Los_Angeles|America/Vancouver",
			"America/Los_Angeles|Canada/Pacific",
			"America/Los_Angeles|Mexico/BajaNorte",
			"America/Los_Angeles|PST8PDT",
			"America/Los_Angeles|US/Pacific",
			"America/Los_Angeles|US/Pacific-New",
			"America/Managua|America/Belize",
			"America/Managua|America/Costa_Rica",
			"America/Managua|America/El_Salvador",
			"America/Managua|America/Guatemala",
			"America/Managua|America/Regina",
			"America/Managua|America/Swift_Current",
			"America/Managua|America/Tegucigalpa",
			"America/Managua|Canada/Saskatchewan",
			"America/Mexico_City|America/Bahia_Banderas",
			"America/Mexico_City|America/Merida",
			"America/Mexico_City|America/Monterrey",
			"America/Mexico_City|Mexico/General",
			"America/New_York|America/Detroit",
			"America/New_York|America/Fort_Wayne",
			"America/New_York|America/Indiana/Indianapolis",
			"America/New_York|America/Indiana/Marengo",
			"America/New_York|America/Indiana/Petersburg",
			"America/New_York|America/Indiana/Vevay",
			"America/New_York|America/Indiana/Vincennes",
			"America/New_York|America/Indiana/Winamac",
			"America/New_York|America/Indianapolis",
			"America/New_York|America/Iqaluit",
			"America/New_York|America/Kentucky/Louisville",
			"America/New_York|America/Kentucky/Monticello",
			"America/New_York|America/Louisville",
			"America/New_York|America/Montreal",
			"America/New_York|America/Nassau",
			"America/New_York|America/Nipigon",
			"America/New_York|America/Pangnirtung",
			"America/New_York|America/Thunder_Bay",
			"America/New_York|America/Toronto",
			"America/New_York|Canada/Eastern",
			"America/New_York|EST5EDT",
			"America/New_York|US/East-Indiana",
			"America/New_York|US/Eastern",
			"America/New_York|US/Michigan",
			"America/Noronha|Atlantic/South_Georgia",
			"America/Noronha|Brazil/DeNoronha",
			"America/Noronha|Etc/GMT+2",
			"America/Panama|America/Atikokan",
			"America/Panama|America/Cayman",
			"America/Panama|America/Coral_Harbour",
			"America/Panama|America/Jamaica",
			"America/Panama|EST",
			"America/Panama|Jamaica",
			"America/Phoenix|America/Creston",
			"America/Phoenix|America/Dawson_Creek",
			"America/Phoenix|America/Hermosillo",
			"America/Phoenix|MST",
			"America/Phoenix|US/Arizona",
			"America/Santiago|Chile/Continental",
			"America/Santo_Domingo|America/Anguilla",
			"America/Santo_Domingo|America/Antigua",
			"America/Santo_Domingo|America/Aruba",
			"America/Santo_Domingo|America/Barbados",
			"America/Santo_Domingo|America/Blanc-Sablon",
			"America/Santo_Domingo|America/Curacao",
			"America/Santo_Domingo|America/Dominica",
			"America/Santo_Domingo|America/Grenada",
			"America/Santo_Domingo|America/Guadeloupe",
			"America/Santo_Domingo|America/Kralendijk",
			"America/Santo_Domingo|America/Lower_Princes",
			"America/Santo_Domingo|America/Marigot",
			"America/Santo_Domingo|America/Martinique",
			"America/Santo_Domingo|America/Montserrat",
			"America/Santo_Domingo|America/Port_of_Spain",
			"America/Santo_Domingo|America/Puerto_Rico",
			"America/Santo_Domingo|America/St_Barthelemy",
			"America/Santo_Domingo|America/St_Kitts",
			"America/Santo_Domingo|America/St_Lucia",
			"America/Santo_Domingo|America/St_Thomas",
			"America/Santo_Domingo|America/St_Vincent",
			"America/Santo_Domingo|America/Tortola",
			"America/Santo_Domingo|America/Virgin",
			"America/Sao_Paulo|Brazil/East",
			"America/St_Johns|Canada/Newfoundland",
			"America/Whitehorse|America/Dawson",
			"America/Whitehorse|Canada/Yukon",
			"Antarctica/Palmer|America/Punta_Arenas",
			"Asia/Baghdad|Antarctica/Syowa",
			"Asia/Baghdad|Asia/Aden",
			"Asia/Baghdad|Asia/Bahrain",
			"Asia/Baghdad|Asia/Kuwait",
			"Asia/Baghdad|Asia/Qatar",
			"Asia/Baghdad|Asia/Riyadh",
			"Asia/Baghdad|Etc/GMT-3",
			"Asia/Baghdad|Europe/Kirov",
			"Asia/Baghdad|Europe/Minsk",
			"Asia/Bangkok|Antarctica/Davis",
			"Asia/Bangkok|Asia/Ho_Chi_Minh",
			"Asia/Bangkok|Asia/Krasnoyarsk",
			"Asia/Bangkok|Asia/Novokuznetsk",
			"Asia/Bangkok|Asia/Phnom_Penh",
			"Asia/Bangkok|Asia/Saigon",
			"Asia/Bangkok|Asia/Vientiane",
			"Asia/Bangkok|Etc/GMT-7",
			"Asia/Bangkok|Indian/Christmas",
			"Asia/Dhaka|Antarctica/Vostok",
			"Asia/Dhaka|Asia/Almaty",
			"Asia/Dhaka|Asia/Bishkek",
			"Asia/Dhaka|Asia/Dacca",
			"Asia/Dhaka|Asia/Kashgar",
			"Asia/Dhaka|Asia/Omsk",
			"Asia/Dhaka|Asia/Qostanay",
			"Asia/Dhaka|Asia/Thimbu",
			"Asia/Dhaka|Asia/Thimphu",
			"Asia/Dhaka|Asia/Urumqi",
			"Asia/Dhaka|Etc/GMT-6",
			"Asia/Dhaka|Indian/Chagos",
			"Asia/Dubai|Asia/Muscat",
			"Asia/Dubai|Asia/Tbilisi",
			"Asia/Dubai|Asia/Yerevan",
			"Asia/Dubai|Etc/GMT-4",
			"Asia/Dubai|Europe/Samara",
			"Asia/Dubai|Indian/Mahe",
			"Asia/Dubai|Indian/Mauritius",
			"Asia/Dubai|Indian/Reunion",
			"Asia/Gaza|Asia/Hebron",
			"Asia/Hong_Kong|Hongkong",
			"Asia/Jakarta|Asia/Pontianak",
			"Asia/Jerusalem|Asia/Tel_Aviv",
			"Asia/Jerusalem|Israel",
			"Asia/Kamchatka|Asia/Anadyr",
			"Asia/Kamchatka|Etc/GMT-12",
			"Asia/Kamchatka|Kwajalein",
			"Asia/Kamchatka|Pacific/Funafuti",
			"Asia/Kamchatka|Pacific/Kwajalein",
			"Asia/Kamchatka|Pacific/Majuro",
			"Asia/Kamchatka|Pacific/Nauru",
			"Asia/Kamchatka|Pacific/Tarawa",
			"Asia/Kamchatka|Pacific/Wake",
			"Asia/Kamchatka|Pacific/Wallis",
			"Asia/Kathmandu|Asia/Katmandu",
			"Asia/Kolkata|Asia/Calcutta",
			"Asia/Kuala_Lumpur|Asia/Brunei",
			"Asia/Kuala_Lumpur|Asia/Irkutsk",
			"Asia/Kuala_Lumpur|Asia/Kuching",
			"Asia/Kuala_Lumpur|Asia/Singapore",
			"Asia/Kuala_Lumpur|Etc/GMT-8",
			"Asia/Kuala_Lumpur|Singapore",
			"Asia/Makassar|Asia/Ujung_Pandang",
			"Asia/Rangoon|Asia/Yangon",
			"Asia/Rangoon|Indian/Cocos",
			"Asia/Seoul|ROK",
			"Asia/Shanghai|Asia/Chongqing",
			"Asia/Shanghai|Asia/Chungking",
			"Asia/Shanghai|Asia/Harbin",
			"Asia/Shanghai|Asia/Macao",
			"Asia/Shanghai|Asia/Macau",
			"Asia/Shanghai|Asia/Taipei",
			"Asia/Shanghai|PRC",
			"Asia/Shanghai|ROC",
			"Asia/Tashkent|Antarctica/Mawson",
			"Asia/Tashkent|Asia/Aqtau",
			"Asia/Tashkent|Asia/Aqtobe",
			"Asia/Tashkent|Asia/Ashgabat",
			"Asia/Tashkent|Asia/Ashkhabad",
			"Asia/Tashkent|Asia/Atyrau",
			"Asia/Tashkent|Asia/Dushanbe",
			"Asia/Tashkent|Asia/Oral",
			"Asia/Tashkent|Asia/Samarkand",
			"Asia/Tashkent|Asia/Yekaterinburg",
			"Asia/Tashkent|Etc/GMT-5",
			"Asia/Tashkent|Indian/Kerguelen",
			"Asia/Tashkent|Indian/Maldives",
			"Asia/Tehran|Iran",
			"Asia/Tokyo|Japan",
			"Asia/Ulaanbaatar|Asia/Choibalsan",
			"Asia/Ulaanbaatar|Asia/Ulan_Bator",
			"Asia/Vladivostok|Antarctica/DumontDUrville",
			"Asia/Vladivostok|Asia/Ust-Nera",
			"Asia/Vladivostok|Etc/GMT-10",
			"Asia/Vladivostok|Pacific/Chuuk",
			"Asia/Vladivostok|Pacific/Port_Moresby",
			"Asia/Vladivostok|Pacific/Truk",
			"Asia/Vladivostok|Pacific/Yap",
			"Asia/Yakutsk|Asia/Dili",
			"Asia/Yakutsk|Asia/Khandyga",
			"Asia/Yakutsk|Etc/GMT-9",
			"Asia/Yakutsk|Pacific/Palau",
			"Atlantic/Azores|America/Scoresbysund",
			"Atlantic/Cape_Verde|Etc/GMT+1",
			"Australia/Adelaide|Australia/Broken_Hill",
			"Australia/Adelaide|Australia/South",
			"Australia/Adelaide|Australia/Yancowinna",
			"Australia/Brisbane|Australia/Lindeman",
			"Australia/Brisbane|Australia/Queensland",
			"Australia/Darwin|Australia/North",
			"Australia/Lord_Howe|Australia/LHI",
			"Australia/Perth|Australia/West",
			"Australia/Sydney|Australia/ACT",
			"Australia/Sydney|Australia/Canberra",
			"Australia/Sydney|Australia/Currie",
			"Australia/Sydney|Australia/Hobart",
			"Australia/Sydney|Australia/Melbourne",
			"Australia/Sydney|Australia/NSW",
			"Australia/Sydney|Australia/Tasmania",
			"Australia/Sydney|Australia/Victoria",
			"Etc/UTC|Etc/UCT",
			"Etc/UTC|Etc/Universal",
			"Etc/UTC|Etc/Zulu",
			"Etc/UTC|UCT",
			"Etc/UTC|UTC",
			"Etc/UTC|Universal",
			"Etc/UTC|Zulu",
			"Europe/Athens|Asia/Nicosia",
			"Europe/Athens|EET",
			"Europe/Athens|Europe/Bucharest",
			"Europe/Athens|Europe/Helsinki",
			"Europe/Athens|Europe/Kiev",
			"Europe/Athens|Europe/Mariehamn",
			"Europe/Athens|Europe/Nicosia",
			"Europe/Athens|Europe/Riga",
			"Europe/Athens|Europe/Sofia",
			"Europe/Athens|Europe/Tallinn",
			"Europe/Athens|Europe/Uzhgorod",
			"Europe/Athens|Europe/Vilnius",
			"Europe/Athens|Europe/Zaporozhye",
			"Europe/Chisinau|Europe/Tiraspol",
			"Europe/Dublin|Eire",
			"Europe/Istanbul|Asia/Istanbul",
			"Europe/Istanbul|Turkey",
			"Europe/Lisbon|Atlantic/Canary",
			"Europe/Lisbon|Atlantic/Faeroe",
			"Europe/Lisbon|Atlantic/Faroe",
			"Europe/Lisbon|Atlantic/Madeira",
			"Europe/Lisbon|Portugal",
			"Europe/Lisbon|WET",
			"Europe/London|Europe/Belfast",
			"Europe/London|Europe/Guernsey",
			"Europe/London|Europe/Isle_of_Man",
			"Europe/London|Europe/Jersey",
			"Europe/London|GB",
			"Europe/London|GB-Eire",
			"Europe/Moscow|Europe/Simferopol",
			"Europe/Moscow|W-SU",
			"Europe/Paris|Africa/Ceuta",
			"Europe/Paris|Arctic/Longyearbyen",
			"Europe/Paris|Atlantic/Jan_Mayen",
			"Europe/Paris|CET",
			"Europe/Paris|Europe/Amsterdam",
			"Europe/Paris|Europe/Andorra",
			"Europe/Paris|Europe/Belgrade",
			"Europe/Paris|Europe/Berlin",
			"Europe/Paris|Europe/Bratislava",
			"Europe/Paris|Europe/Brussels",
			"Europe/Paris|Europe/Budapest",
			"Europe/Paris|Europe/Busingen",
			"Europe/Paris|Europe/Copenhagen",
			"Europe/Paris|Europe/Gibraltar",
			"Europe/Paris|Europe/Ljubljana",
			"Europe/Paris|Europe/Luxembourg",
			"Europe/Paris|Europe/Madrid",
			"Europe/Paris|Europe/Malta",
			"Europe/Paris|Europe/Monaco",
			"Europe/Paris|Europe/Oslo",
			"Europe/Paris|Europe/Podgorica",
			"Europe/Paris|Europe/Prague",
			"Europe/Paris|Europe/Rome",
			"Europe/Paris|Europe/San_Marino",
			"Europe/Paris|Europe/Sarajevo",
			"Europe/Paris|Europe/Skopje",
			"Europe/Paris|Europe/Stockholm",
			"Europe/Paris|Europe/Tirane",
			"Europe/Paris|Europe/Vaduz",
			"Europe/Paris|Europe/Vatican",
			"Europe/Paris|Europe/Vienna",
			"Europe/Paris|Europe/Warsaw",
			"Europe/Paris|Europe/Zagreb",
			"Europe/Paris|Europe/Zurich",
			"Europe/Paris|Poland",
			"Europe/Ulyanovsk|Europe/Astrakhan",
			"Pacific/Auckland|Antarctica/McMurdo",
			"Pacific/Auckland|Antarctica/South_Pole",
			"Pacific/Auckland|NZ",
			"Pacific/Bougainville|Antarctica/Macquarie",
			"Pacific/Bougainville|Asia/Srednekolymsk",
			"Pacific/Bougainville|Etc/GMT-11",
			"Pacific/Bougainville|Pacific/Efate",
			"Pacific/Bougainville|Pacific/Guadalcanal",
			"Pacific/Bougainville|Pacific/Kosrae",
			"Pacific/Bougainville|Pacific/Noumea",
			"Pacific/Bougainville|Pacific/Pohnpei",
			"Pacific/Bougainville|Pacific/Ponape",
			"Pacific/Chatham|NZ-CHAT",
			"Pacific/Easter|Chile/EasterIsland",
			"Pacific/Fakaofo|Etc/GMT-13",
			"Pacific/Fakaofo|Pacific/Enderbury",
			"Pacific/Galapagos|Etc/GMT+6",
			"Pacific/Gambier|Etc/GMT+9",
			"Pacific/Guam|Pacific/Saipan",
			"Pacific/Honolulu|HST",
			"Pacific/Honolulu|Pacific/Johnston",
			"Pacific/Honolulu|US/Hawaii",
			"Pacific/Kiritimati|Etc/GMT-14",
			"Pacific/Niue|Etc/GMT+11",
			"Pacific/Pago_Pago|Pacific/Midway",
			"Pacific/Pago_Pago|Pacific/Samoa",
			"Pacific/Pago_Pago|US/Samoa",
			"Pacific/Pitcairn|Etc/GMT+8",
			"Pacific/Tahiti|Etc/GMT+10",
			"Pacific/Tahiti|Pacific/Rarotonga"
		],
		"countries": [
			"AD|Europe/Andorra",
			"AE|Asia/Dubai",
			"AF|Asia/Kabul",
			"AG|America/Port_of_Spain America/Antigua",
			"AI|America/Port_of_Spain America/Anguilla",
			"AL|Europe/Tirane",
			"AM|Asia/Yerevan",
			"AO|Africa/Lagos Africa/Luanda",
			"AQ|Antarctica/Casey Antarctica/Davis Antarctica/DumontDUrville Antarctica/Mawson Antarctica/Palmer Antarctica/Rothera Antarctica/Syowa Antarctica/Troll Antarctica/Vostok Pacific/Auckland Antarctica/McMurdo",
			"AR|America/Argentina/Buenos_Aires America/Argentina/Cordoba America/Argentina/Salta America/Argentina/Jujuy America/Argentina/Tucuman America/Argentina/Catamarca America/Argentina/La_Rioja America/Argentina/San_Juan America/Argentina/Mendoza America/Argentina/San_Luis America/Argentina/Rio_Gallegos America/Argentina/Ushuaia",
			"AS|Pacific/Pago_Pago",
			"AT|Europe/Vienna",
			"AU|Australia/Lord_Howe Antarctica/Macquarie Australia/Hobart Australia/Currie Australia/Melbourne Australia/Sydney Australia/Broken_Hill Australia/Brisbane Australia/Lindeman Australia/Adelaide Australia/Darwin Australia/Perth Australia/Eucla",
			"AW|America/Curacao America/Aruba",
			"AX|Europe/Helsinki Europe/Mariehamn",
			"AZ|Asia/Baku",
			"BA|Europe/Belgrade Europe/Sarajevo",
			"BB|America/Barbados",
			"BD|Asia/Dhaka",
			"BE|Europe/Brussels",
			"BF|Africa/Abidjan Africa/Ouagadougou",
			"BG|Europe/Sofia",
			"BH|Asia/Qatar Asia/Bahrain",
			"BI|Africa/Maputo Africa/Bujumbura",
			"BJ|Africa/Lagos Africa/Porto-Novo",
			"BL|America/Port_of_Spain America/St_Barthelemy",
			"BM|Atlantic/Bermuda",
			"BN|Asia/Brunei",
			"BO|America/La_Paz",
			"BQ|America/Curacao America/Kralendijk",
			"BR|America/Noronha America/Belem America/Fortaleza America/Recife America/Araguaina America/Maceio America/Bahia America/Sao_Paulo America/Campo_Grande America/Cuiaba America/Santarem America/Porto_Velho America/Boa_Vista America/Manaus America/Eirunepe America/Rio_Branco",
			"BS|America/Nassau",
			"BT|Asia/Thimphu",
			"BW|Africa/Maputo Africa/Gaborone",
			"BY|Europe/Minsk",
			"BZ|America/Belize",
			"CA|America/St_Johns America/Halifax America/Glace_Bay America/Moncton America/Goose_Bay America/Blanc-Sablon America/Toronto America/Nipigon America/Thunder_Bay America/Iqaluit America/Pangnirtung America/Atikokan America/Winnipeg America/Rainy_River America/Resolute America/Rankin_Inlet America/Regina America/Swift_Current America/Edmonton America/Cambridge_Bay America/Yellowknife America/Inuvik America/Creston America/Dawson_Creek America/Fort_Nelson America/Vancouver America/Whitehorse America/Dawson",
			"CC|Indian/Cocos",
			"CD|Africa/Maputo Africa/Lagos Africa/Kinshasa Africa/Lubumbashi",
			"CF|Africa/Lagos Africa/Bangui",
			"CG|Africa/Lagos Africa/Brazzaville",
			"CH|Europe/Zurich",
			"CI|Africa/Abidjan",
			"CK|Pacific/Rarotonga",
			"CL|America/Santiago America/Punta_Arenas Pacific/Easter",
			"CM|Africa/Lagos Africa/Douala",
			"CN|Asia/Shanghai Asia/Urumqi",
			"CO|America/Bogota",
			"CR|America/Costa_Rica",
			"CU|America/Havana",
			"CV|Atlantic/Cape_Verde",
			"CW|America/Curacao",
			"CX|Indian/Christmas",
			"CY|Asia/Nicosia Asia/Famagusta",
			"CZ|Europe/Prague",
			"DE|Europe/Zurich Europe/Berlin Europe/Busingen",
			"DJ|Africa/Nairobi Africa/Djibouti",
			"DK|Europe/Copenhagen",
			"DM|America/Port_of_Spain America/Dominica",
			"DO|America/Santo_Domingo",
			"DZ|Africa/Algiers",
			"EC|America/Guayaquil Pacific/Galapagos",
			"EE|Europe/Tallinn",
			"EG|Africa/Cairo",
			"EH|Africa/El_Aaiun",
			"ER|Africa/Nairobi Africa/Asmara",
			"ES|Europe/Madrid Africa/Ceuta Atlantic/Canary",
			"ET|Africa/Nairobi Africa/Addis_Ababa",
			"FI|Europe/Helsinki",
			"FJ|Pacific/Fiji",
			"FK|Atlantic/Stanley",
			"FM|Pacific/Chuuk Pacific/Pohnpei Pacific/Kosrae",
			"FO|Atlantic/Faroe",
			"FR|Europe/Paris",
			"GA|Africa/Lagos Africa/Libreville",
			"GB|Europe/London",
			"GD|America/Port_of_Spain America/Grenada",
			"GE|Asia/Tbilisi",
			"GF|America/Cayenne",
			"GG|Europe/London Europe/Guernsey",
			"GH|Africa/Accra",
			"GI|Europe/Gibraltar",
			"GL|America/Godthab America/Danmarkshavn America/Scoresbysund America/Thule",
			"GM|Africa/Abidjan Africa/Banjul",
			"GN|Africa/Abidjan Africa/Conakry",
			"GP|America/Port_of_Spain America/Guadeloupe",
			"GQ|Africa/Lagos Africa/Malabo",
			"GR|Europe/Athens",
			"GS|Atlantic/South_Georgia",
			"GT|America/Guatemala",
			"GU|Pacific/Guam",
			"GW|Africa/Bissau",
			"GY|America/Guyana",
			"HK|Asia/Hong_Kong",
			"HN|America/Tegucigalpa",
			"HR|Europe/Belgrade Europe/Zagreb",
			"HT|America/Port-au-Prince",
			"HU|Europe/Budapest",
			"ID|Asia/Jakarta Asia/Pontianak Asia/Makassar Asia/Jayapura",
			"IE|Europe/Dublin",
			"IL|Asia/Jerusalem",
			"IM|Europe/London Europe/Isle_of_Man",
			"IN|Asia/Kolkata",
			"IO|Indian/Chagos",
			"IQ|Asia/Baghdad",
			"IR|Asia/Tehran",
			"IS|Atlantic/Reykjavik",
			"IT|Europe/Rome",
			"JE|Europe/London Europe/Jersey",
			"JM|America/Jamaica",
			"JO|Asia/Amman",
			"JP|Asia/Tokyo",
			"KE|Africa/Nairobi",
			"KG|Asia/Bishkek",
			"KH|Asia/Bangkok Asia/Phnom_Penh",
			"KI|Pacific/Tarawa Pacific/Enderbury Pacific/Kiritimati",
			"KM|Africa/Nairobi Indian/Comoro",
			"KN|America/Port_of_Spain America/St_Kitts",
			"KP|Asia/Pyongyang",
			"KR|Asia/Seoul",
			"KW|Asia/Riyadh Asia/Kuwait",
			"KY|America/Panama America/Cayman",
			"KZ|Asia/Almaty Asia/Qyzylorda Asia/Qostanay Asia/Aqtobe Asia/Aqtau Asia/Atyrau Asia/Oral",
			"LA|Asia/Bangkok Asia/Vientiane",
			"LB|Asia/Beirut",
			"LC|America/Port_of_Spain America/St_Lucia",
			"LI|Europe/Zurich Europe/Vaduz",
			"LK|Asia/Colombo",
			"LR|Africa/Monrovia",
			"LS|Africa/Johannesburg Africa/Maseru",
			"LT|Europe/Vilnius",
			"LU|Europe/Luxembourg",
			"LV|Europe/Riga",
			"LY|Africa/Tripoli",
			"MA|Africa/Casablanca",
			"MC|Europe/Monaco",
			"MD|Europe/Chisinau",
			"ME|Europe/Belgrade Europe/Podgorica",
			"MF|America/Port_of_Spain America/Marigot",
			"MG|Africa/Nairobi Indian/Antananarivo",
			"MH|Pacific/Majuro Pacific/Kwajalein",
			"MK|Europe/Belgrade Europe/Skopje",
			"ML|Africa/Abidjan Africa/Bamako",
			"MM|Asia/Yangon",
			"MN|Asia/Ulaanbaatar Asia/Hovd Asia/Choibalsan",
			"MO|Asia/Macau",
			"MP|Pacific/Guam Pacific/Saipan",
			"MQ|America/Martinique",
			"MR|Africa/Abidjan Africa/Nouakchott",
			"MS|America/Port_of_Spain America/Montserrat",
			"MT|Europe/Malta",
			"MU|Indian/Mauritius",
			"MV|Indian/Maldives",
			"MW|Africa/Maputo Africa/Blantyre",
			"MX|America/Mexico_City America/Cancun America/Merida America/Monterrey America/Matamoros America/Mazatlan America/Chihuahua America/Ojinaga America/Hermosillo America/Tijuana America/Bahia_Banderas",
			"MY|Asia/Kuala_Lumpur Asia/Kuching",
			"MZ|Africa/Maputo",
			"NA|Africa/Windhoek",
			"NC|Pacific/Noumea",
			"NE|Africa/Lagos Africa/Niamey",
			"NF|Pacific/Norfolk",
			"NG|Africa/Lagos",
			"NI|America/Managua",
			"NL|Europe/Amsterdam",
			"NO|Europe/Oslo",
			"NP|Asia/Kathmandu",
			"NR|Pacific/Nauru",
			"NU|Pacific/Niue",
			"NZ|Pacific/Auckland Pacific/Chatham",
			"OM|Asia/Dubai Asia/Muscat",
			"PA|America/Panama",
			"PE|America/Lima",
			"PF|Pacific/Tahiti Pacific/Marquesas Pacific/Gambier",
			"PG|Pacific/Port_Moresby Pacific/Bougainville",
			"PH|Asia/Manila",
			"PK|Asia/Karachi",
			"PL|Europe/Warsaw",
			"PM|America/Miquelon",
			"PN|Pacific/Pitcairn",
			"PR|America/Puerto_Rico",
			"PS|Asia/Gaza Asia/Hebron",
			"PT|Europe/Lisbon Atlantic/Madeira Atlantic/Azores",
			"PW|Pacific/Palau",
			"PY|America/Asuncion",
			"QA|Asia/Qatar",
			"RE|Indian/Reunion",
			"RO|Europe/Bucharest",
			"RS|Europe/Belgrade",
			"RU|Europe/Kaliningrad Europe/Moscow Europe/Simferopol Europe/Kirov Europe/Astrakhan Europe/Volgograd Europe/Saratov Europe/Ulyanovsk Europe/Samara Asia/Yekaterinburg Asia/Omsk Asia/Novosibirsk Asia/Barnaul Asia/Tomsk Asia/Novokuznetsk Asia/Krasnoyarsk Asia/Irkutsk Asia/Chita Asia/Yakutsk Asia/Khandyga Asia/Vladivostok Asia/Ust-Nera Asia/Magadan Asia/Sakhalin Asia/Srednekolymsk Asia/Kamchatka Asia/Anadyr",
			"RW|Africa/Maputo Africa/Kigali",
			"SA|Asia/Riyadh",
			"SB|Pacific/Guadalcanal",
			"SC|Indian/Mahe",
			"SD|Africa/Khartoum",
			"SE|Europe/Stockholm",
			"SG|Asia/Singapore",
			"SH|Africa/Abidjan Atlantic/St_Helena",
			"SI|Europe/Belgrade Europe/Ljubljana",
			"SJ|Europe/Oslo Arctic/Longyearbyen",
			"SK|Europe/Prague Europe/Bratislava",
			"SL|Africa/Abidjan Africa/Freetown",
			"SM|Europe/Rome Europe/San_Marino",
			"SN|Africa/Abidjan Africa/Dakar",
			"SO|Africa/Nairobi Africa/Mogadishu",
			"SR|America/Paramaribo",
			"SS|Africa/Juba",
			"ST|Africa/Sao_Tome",
			"SV|America/El_Salvador",
			"SX|America/Curacao America/Lower_Princes",
			"SY|Asia/Damascus",
			"SZ|Africa/Johannesburg Africa/Mbabane",
			"TC|America/Grand_Turk",
			"TD|Africa/Ndjamena",
			"TF|Indian/Reunion Indian/Kerguelen",
			"TG|Africa/Abidjan Africa/Lome",
			"TH|Asia/Bangkok",
			"TJ|Asia/Dushanbe",
			"TK|Pacific/Fakaofo",
			"TL|Asia/Dili",
			"TM|Asia/Ashgabat",
			"TN|Africa/Tunis",
			"TO|Pacific/Tongatapu",
			"TR|Europe/Istanbul",
			"TT|America/Port_of_Spain",
			"TV|Pacific/Funafuti",
			"TW|Asia/Taipei",
			"TZ|Africa/Nairobi Africa/Dar_es_Salaam",
			"UA|Europe/Simferopol Europe/Kiev Europe/Uzhgorod Europe/Zaporozhye",
			"UG|Africa/Nairobi Africa/Kampala",
			"UM|Pacific/Pago_Pago Pacific/Wake Pacific/Honolulu Pacific/Midway",
			"US|America/New_York America/Detroit America/Kentucky/Louisville America/Kentucky/Monticello America/Indiana/Indianapolis America/Indiana/Vincennes America/Indiana/Winamac America/Indiana/Marengo America/Indiana/Petersburg America/Indiana/Vevay America/Chicago America/Indiana/Tell_City America/Indiana/Knox America/Menominee America/North_Dakota/Center America/North_Dakota/New_Salem America/North_Dakota/Beulah America/Denver America/Boise America/Phoenix America/Los_Angeles America/Anchorage America/Juneau America/Sitka America/Metlakatla America/Yakutat America/Nome America/Adak Pacific/Honolulu",
			"UY|America/Montevideo",
			"UZ|Asia/Samarkand Asia/Tashkent",
			"VA|Europe/Rome Europe/Vatican",
			"VC|America/Port_of_Spain America/St_Vincent",
			"VE|America/Caracas",
			"VG|America/Port_of_Spain America/Tortola",
			"VI|America/Port_of_Spain America/St_Thomas",
			"VN|Asia/Bangkok Asia/Ho_Chi_Minh",
			"VU|Pacific/Efate",
			"WF|Pacific/Wallis",
			"WS|Pacific/Apia",
			"YE|Asia/Riyadh Asia/Aden",
			"YT|Africa/Nairobi Indian/Mayotte",
			"ZA|Africa/Johannesburg",
			"ZM|Africa/Maputo Africa/Lusaka",
			"ZW|Africa/Maputo Africa/Harare"
		]
	});


	return moment;
}));
//! moment.js locale configuration
//! locale : Spanish [es]
//! author : Julio Napurí : https://github.com/julionc

;(function (global, factory) {
   typeof exports === 'object' && typeof module !== 'undefined'
       && typeof require === 'function' ? factory(require('../moment')) :
   typeof define === 'function' && define.amd ? define(['../moment'], factory) :
   factory(global.moment)
}(this, (function (moment) { 'use strict';

    //! moment.js locale configuration

    var monthsShortDot = 'ene._feb._mar._abr._may._jun._jul._ago._sep._oct._nov._dic.'.split(
            '_'
        ),
        monthsShort = 'ene_feb_mar_abr_may_jun_jul_ago_sep_oct_nov_dic'.split('_'),
        monthsParse = [
            /^ene/i,
            /^feb/i,
            /^mar/i,
            /^abr/i,
            /^may/i,
            /^jun/i,
            /^jul/i,
            /^ago/i,
            /^sep/i,
            /^oct/i,
            /^nov/i,
            /^dic/i,
        ],
        monthsRegex = /^(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre|ene\.?|feb\.?|mar\.?|abr\.?|may\.?|jun\.?|jul\.?|ago\.?|sep\.?|oct\.?|nov\.?|dic\.?)/i;

    var es = moment.defineLocale('es', {
        months: 'enero_febrero_marzo_abril_mayo_junio_julio_agosto_septiembre_octubre_noviembre_diciembre'.split(
            '_'
        ),
        monthsShort: function (m, format) {
            if (!m) {
                return monthsShortDot;
            } else if (/-MMM-/.test(format)) {
                return monthsShort[m.month()];
            } else {
                return monthsShortDot[m.month()];
            }
        },
        monthsRegex: monthsRegex,
        monthsShortRegex: monthsRegex,
        monthsStrictRegex: /^(enero|febrero|marzo|abril|mayo|junio|julio|agosto|septiembre|octubre|noviembre|diciembre)/i,
        monthsShortStrictRegex: /^(ene\.?|feb\.?|mar\.?|abr\.?|may\.?|jun\.?|jul\.?|ago\.?|sep\.?|oct\.?|nov\.?|dic\.?)/i,
        monthsParse: monthsParse,
        longMonthsParse: monthsParse,
        shortMonthsParse: monthsParse,
        weekdays: 'domingo_lunes_martes_miércoles_jueves_viernes_sábado'.split('_'),
        weekdaysShort: 'dom._lun._mar._mié._jue._vie._sáb.'.split('_'),
        weekdaysMin: 'do_lu_ma_mi_ju_vi_sá'.split('_'),
        weekdaysParseExact: true,
        longDateFormat: {
            LT: 'H:mm',
            LTS: 'H:mm:ss',
            L: 'DD/MM/YYYY',
            LL: 'D [de] MMMM [de] YYYY',
            LLL: 'D [de] MMMM [de] YYYY H:mm',
            LLLL: 'dddd, D [de] MMMM [de] YYYY H:mm',
        },
        calendar: {
            sameDay: function () {
                return '[hoy a la' + (this.hours() !== 1 ? 's' : '') + '] LT';
            },
            nextDay: function () {
                return '[mañana a la' + (this.hours() !== 1 ? 's' : '') + '] LT';
            },
            nextWeek: function () {
                return 'dddd [a la' + (this.hours() !== 1 ? 's' : '') + '] LT';
            },
            lastDay: function () {
                return '[ayer a la' + (this.hours() !== 1 ? 's' : '') + '] LT';
            },
            lastWeek: function () {
                return (
                    '[el] dddd [pasado a la' +
                    (this.hours() !== 1 ? 's' : '') +
                    '] LT'
                );
            },
            sameElse: 'L',
        },
        relativeTime: {
            future: 'en %s',
            past: 'hace %s',
            s: 'unos segundos',
            ss: '%d segundos',
            m: 'un minuto',
            mm: '%d minutos',
            h: 'una hora',
            hh: '%d horas',
            d: 'un día',
            dd: '%d días',
            w: 'una semana',
            ww: '%d semanas',
            M: 'un mes',
            MM: '%d meses',
            y: 'un año',
            yy: '%d años',
        },
        dayOfMonthOrdinalParse: /\d{1,2}º/,
        ordinal: '%dº',
        week: {
            dow: 1, // Monday is the first day of the week.
            doy: 4, // The week that contains Jan 4th is the first week of the year.
        },
        invalidDate: 'Fecha inválida',
    });

    return es;

})));

// moment-timezone-localization for lang code: es

;(function (global, factory) {
   typeof exports === 'object' && typeof module !== 'undefined'
       && typeof require === 'function' ? factory(require('../moment')) :
   typeof define === 'function' && define.amd ? define(['../moment'], factory) :
   factory(global.moment)
}(this, (function (moment) { 'use strict';


moment.tz.localizedNames = function() {
  return [{"value":"Africa/Abidjan","name":"Abiyán","id":"Africa/Abidjan"},{"value":"Africa/Accra","name":"Acra","id":"Africa/Accra"},{"value":"Africa/Addis_Ababa","name":"Addis Abeba","id":"Africa/Addis_Ababa"},{"value":"Africa/Algiers","name":"Argel","id":"Africa/Algiers"},{"value":"Africa/Asmera","name":"Asmara","id":"Africa/Asmera"},{"value":"Africa/Bamako","name":"Bamako","id":"Africa/Bamako"},{"value":"Africa/Bangui","name":"Bangui","id":"Africa/Bangui"},{"value":"Africa/Banjul","name":"Banjul","id":"Africa/Banjul"},{"value":"Africa/Bissau","name":"Bisáu","id":"Africa/Bissau"},{"value":"Africa/Blantyre","name":"Blantyre","id":"Africa/Blantyre"},{"value":"Africa/Brazzaville","name":"Brazzaville","id":"Africa/Brazzaville"},{"value":"Africa/Bujumbura","name":"Bujumbura","id":"Africa/Bujumbura"},{"value":"Africa/Cairo","name":"El Cairo","id":"Africa/Cairo"},{"value":"Africa/Casablanca","name":"Casablanca","id":"Africa/Casablanca"},{"value":"Africa/Ceuta","name":"Ceuta","id":"Africa/Ceuta"},{"value":"Africa/Conakry","name":"Conakry","id":"Africa/Conakry"},{"value":"Africa/Dakar","name":"Dakar","id":"Africa/Dakar"},{"value":"Africa/Dar_es_Salaam","name":"Dar es Salaam","id":"Africa/Dar_es_Salaam"},{"value":"Africa/Djibouti","name":"Yibuti","id":"Africa/Djibouti"},{"value":"Africa/Douala","name":"Duala","id":"Africa/Douala"},{"value":"Africa/El_Aaiun","name":"El Aaiún","id":"Africa/El_Aaiun"},{"value":"Africa/Freetown","name":"Freetown","id":"Africa/Freetown"},{"value":"Africa/Gaborone","name":"Gaborone","id":"Africa/Gaborone"},{"value":"Africa/Harare","name":"Harare","id":"Africa/Harare"},{"value":"Africa/Johannesburg","name":"Johannesburgo","id":"Africa/Johannesburg"},{"value":"Africa/Juba","name":"Juba","id":"Africa/Juba"},{"value":"Africa/Kampala","name":"Kampala","id":"Africa/Kampala"},{"value":"Africa/Khartoum","name":"Jartún","id":"Africa/Khartoum"},{"value":"Africa/Kigali","name":"Kigali","id":"Africa/Kigali"},{"value":"Africa/Kinshasa","name":"Kinshasa","id":"Africa/Kinshasa"},{"value":"Africa/Lagos","name":"Lagos","id":"Africa/Lagos"},{"value":"Africa/Libreville","name":"Libreville","id":"Africa/Libreville"},{"value":"Africa/Lome","name":"Lomé","id":"Africa/Lome"},{"value":"Africa/Luanda","name":"Luanda","id":"Africa/Luanda"},{"value":"Africa/Lubumbashi","name":"Lubumbashi","id":"Africa/Lubumbashi"},{"value":"Africa/Lusaka","name":"Lusaka","id":"Africa/Lusaka"},{"value":"Africa/Malabo","name":"Malabo","id":"Africa/Malabo"},{"value":"Africa/Maputo","name":"Maputo","id":"Africa/Maputo"},{"value":"Africa/Maseru","name":"Maseru","id":"Africa/Maseru"},{"value":"Africa/Mbabane","name":"Mbabane","id":"Africa/Mbabane"},{"value":"Africa/Mogadishu","name":"Mogadiscio","id":"Africa/Mogadishu"},{"value":"Africa/Monrovia","name":"Monrovia","id":"Africa/Monrovia"},{"value":"Africa/Nairobi","name":"Nairobi","id":"Africa/Nairobi"},{"value":"Africa/Ndjamena","name":"Yamena","id":"Africa/Ndjamena"},{"value":"Africa/Niamey","name":"Niamey","id":"Africa/Niamey"},{"value":"Africa/Nouakchott","name":"Nuakchot","id":"Africa/Nouakchott"},{"value":"Africa/Ouagadougou","name":"Uagadugú","id":"Africa/Ouagadougou"},{"value":"Africa/Porto-Novo","name":"Portonovo","id":"Africa/Porto-Novo"},{"value":"Africa/Sao_Tome","name":"Santo Tomé","id":"Africa/Sao_Tome"},{"value":"Africa/Tripoli","name":"Trípoli","id":"Africa/Tripoli"},{"value":"Africa/Tunis","name":"Túnez","id":"Africa/Tunis"},{"value":"Africa/Windhoek","name":"Windhoek","id":"Africa/Windhoek"},{"value":"America/Adak","name":"Adak","id":"America/Adak"},{"value":"America/Anchorage","name":"Anchorage","id":"America/Anchorage"},{"value":"America/Anguilla","name":"Anguila","id":"America/Anguilla"},{"value":"America/Antigua","name":"Antigua","id":"America/Antigua"},{"value":"America/Araguaina","name":"Araguaína","id":"America/Araguaina"},{"value":"America/Argentina/La_Rioja","name":"La Rioja","id":"America/Argentina/La_Rioja"},{"value":"America/Argentina/Rio_Gallegos","name":"Río Gallegos","id":"America/Argentina/Rio_Gallegos"},{"value":"America/Argentina/Salta","name":"Salta","id":"America/Argentina/Salta"},{"value":"America/Argentina/San_Juan","name":"San Juan","id":"America/Argentina/San_Juan"},{"value":"America/Argentina/San_Luis","name":"San Luis","id":"America/Argentina/San_Luis"},{"value":"America/Argentina/Tucuman","name":"Tucumán","id":"America/Argentina/Tucuman"},{"value":"America/Argentina/Ushuaia","name":"Ushuaia","id":"America/Argentina/Ushuaia"},{"value":"America/Aruba","name":"Aruba","id":"America/Aruba"},{"value":"America/Asuncion","name":"Asunción","id":"America/Asuncion"},{"value":"America/Bahia","name":"Bahía","id":"America/Bahia"},{"value":"America/Bahia_Banderas","name":"Bahía de Banderas","id":"America/Bahia_Banderas"},{"value":"America/Barbados","name":"Barbados","id":"America/Barbados"},{"value":"America/Belem","name":"Belén","id":"America/Belem"},{"value":"America/Belize","name":"Belice","id":"America/Belize"},{"value":"America/Blanc-Sablon","name":"Blanc-Sablon","id":"America/Blanc-Sablon"},{"value":"America/Boa_Vista","name":"Boa Vista","id":"America/Boa_Vista"},{"value":"America/Bogota","name":"Bogotá","id":"America/Bogota"},{"value":"America/Boise","name":"Boise","id":"America/Boise"},{"value":"America/Buenos_Aires","name":"Buenos Aires","id":"America/Buenos_Aires"},{"value":"America/Cambridge_Bay","name":"Cambridge Bay","id":"America/Cambridge_Bay"},{"value":"America/Campo_Grande","name":"Campo Grande","id":"America/Campo_Grande"},{"value":"America/Cancun","name":"Cancún","id":"America/Cancun"},{"value":"America/Caracas","name":"Caracas","id":"America/Caracas"},{"value":"America/Catamarca","name":"Catamarca","id":"America/Catamarca"},{"value":"America/Cayenne","name":"Cayena","id":"America/Cayenne"},{"value":"America/Cayman","name":"Caimán","id":"America/Cayman"},{"value":"America/Chicago","name":"Chicago","id":"America/Chicago"},{"value":"America/Chihuahua","name":"Chihuahua","id":"America/Chihuahua"},{"value":"America/Coral_Harbour","name":"Atikokan","id":"America/Coral_Harbour"},{"value":"America/Cordoba","name":"Córdoba","id":"America/Cordoba"},{"value":"America/Costa_Rica","name":"Costa Rica","id":"America/Costa_Rica"},{"value":"America/Creston","name":"Creston","id":"America/Creston"},{"value":"America/Cuiaba","name":"Cuiabá","id":"America/Cuiaba"},{"value":"America/Curacao","name":"Curazao","id":"America/Curacao"},{"value":"America/Danmarkshavn","name":"Danmarkshavn","id":"America/Danmarkshavn"},{"value":"America/Dawson","name":"Dawson","id":"America/Dawson"},{"value":"America/Dawson_Creek","name":"Dawson Creek","id":"America/Dawson_Creek"},{"value":"America/Denver","name":"Denver","id":"America/Denver"},{"value":"America/Detroit","name":"Detroit","id":"America/Detroit"},{"value":"America/Dominica","name":"Dominica","id":"America/Dominica"},{"value":"America/Edmonton","name":"Edmonton","id":"America/Edmonton"},{"value":"America/Eirunepe","name":"Eirunepé","id":"America/Eirunepe"},{"value":"America/El_Salvador","name":"El Salvador","id":"America/El_Salvador"},{"value":"America/Fort_Nelson","name":"Fort Nelson","id":"America/Fort_Nelson"},{"value":"America/Fortaleza","name":"Fortaleza","id":"America/Fortaleza"},{"value":"America/Glace_Bay","name":"Glace Bay","id":"America/Glace_Bay"},{"value":"America/Godthab","name":"Nuuk","id":"America/Godthab"},{"value":"America/Goose_Bay","name":"Goose Bay","id":"America/Goose_Bay"},{"value":"America/Grand_Turk","name":"Gran Turca","id":"America/Grand_Turk"},{"value":"America/Grenada","name":"Granada","id":"America/Grenada"},{"value":"America/Guadeloupe","name":"Guadalupe","id":"America/Guadeloupe"},{"value":"America/Guatemala","name":"Guatemala","id":"America/Guatemala"},{"value":"America/Guayaquil","name":"Guayaquil","id":"America/Guayaquil"},{"value":"America/Guyana","name":"Guyana","id":"America/Guyana"},{"value":"America/Halifax","name":"Halifax","id":"America/Halifax"},{"value":"America/Havana","name":"La Habana","id":"America/Havana"},{"value":"America/Hermosillo","name":"Hermosillo","id":"America/Hermosillo"},{"value":"America/Indiana/Knox","name":"Knox, Indiana","id":"America/Indiana/Knox"},{"value":"America/Indiana/Marengo","name":"Marengo, Indiana","id":"America/Indiana/Marengo"},{"value":"America/Indiana/Petersburg","name":"Petersburg, Indiana","id":"America/Indiana/Petersburg"},{"value":"America/Indiana/Tell_City","name":"Tell City, Indiana","id":"America/Indiana/Tell_City"},{"value":"America/Indiana/Vevay","name":"Vevay, Indiana","id":"America/Indiana/Vevay"},{"value":"America/Indiana/Vincennes","name":"Vincennes, Indiana","id":"America/Indiana/Vincennes"},{"value":"America/Indiana/Winamac","name":"Winamac, Indiana","id":"America/Indiana/Winamac"},{"value":"America/Indianapolis","name":"Indianápolis","id":"America/Indianapolis"},{"value":"America/Inuvik","name":"Inuvik","id":"America/Inuvik"},{"value":"America/Iqaluit","name":"Iqaluit","id":"America/Iqaluit"},{"value":"America/Jamaica","name":"Jamaica","id":"America/Jamaica"},{"value":"America/Jujuy","name":"Jujuy","id":"America/Jujuy"},{"value":"America/Juneau","name":"Juneau","id":"America/Juneau"},{"value":"America/Kentucky/Monticello","name":"Monticello, Kentucky","id":"America/Kentucky/Monticello"},{"value":"America/Kralendijk","name":"Kralendijk","id":"America/Kralendijk"},{"value":"America/La_Paz","name":"La Paz","id":"America/La_Paz"},{"value":"America/Lima","name":"Lima","id":"America/Lima"},{"value":"America/Los_Angeles","name":"Los Ángeles","id":"America/Los_Angeles"},{"value":"America/Louisville","name":"Louisville","id":"America/Louisville"},{"value":"America/Lower_Princes","name":"Lower Prince’s Quarter","id":"America/Lower_Princes"},{"value":"America/Maceio","name":"Maceió","id":"America/Maceio"},{"value":"America/Managua","name":"Managua","id":"America/Managua"},{"value":"America/Manaus","name":"Manaos","id":"America/Manaus"},{"value":"America/Marigot","name":"Marigot","id":"America/Marigot"},{"value":"America/Martinique","name":"Martinica","id":"America/Martinique"},{"value":"America/Matamoros","name":"Matamoros","id":"America/Matamoros"},{"value":"America/Mazatlan","name":"Mazatlán","id":"America/Mazatlan"},{"value":"America/Mendoza","name":"Mendoza","id":"America/Mendoza"},{"value":"America/Menominee","name":"Menominee","id":"America/Menominee"},{"value":"America/Merida","name":"Mérida","id":"America/Merida"},{"value":"America/Metlakatla","name":"Metlakatla","id":"America/Metlakatla"},{"value":"America/Mexico_City","name":"Ciudad de México","id":"America/Mexico_City"},{"value":"America/Miquelon","name":"Miquelón","id":"America/Miquelon"},{"value":"America/Moncton","name":"Moncton","id":"America/Moncton"},{"value":"America/Monterrey","name":"Monterrey","id":"America/Monterrey"},{"value":"America/Montevideo","name":"Montevideo","id":"America/Montevideo"},{"value":"America/Montserrat","name":"Montserrat","id":"America/Montserrat"},{"value":"America/Nassau","name":"Nassau","id":"America/Nassau"},{"value":"America/New_York","name":"Nueva York","id":"America/New_York"},{"value":"America/Nipigon","name":"Nipigon","id":"America/Nipigon"},{"value":"America/Nome","name":"Nome","id":"America/Nome"},{"value":"America/Noronha","name":"Noronha","id":"America/Noronha"},{"value":"America/North_Dakota/Beulah","name":"Beulah, Dakota del Norte","id":"America/North_Dakota/Beulah"},{"value":"America/North_Dakota/Center","name":"Center, Dakota del Norte","id":"America/North_Dakota/Center"},{"value":"America/North_Dakota/New_Salem","name":"New Salem, Dakota del Norte","id":"America/North_Dakota/New_Salem"},{"value":"America/Ojinaga","name":"Ojinaga","id":"America/Ojinaga"},{"value":"America/Panama","name":"Panamá","id":"America/Panama"},{"value":"America/Pangnirtung","name":"Pangnirtung","id":"America/Pangnirtung"},{"value":"America/Paramaribo","name":"Paramaribo","id":"America/Paramaribo"},{"value":"America/Phoenix","name":"Phoenix","id":"America/Phoenix"},{"value":"America/Port-au-Prince","name":"Puerto Príncipe","id":"America/Port-au-Prince"},{"value":"America/Port_of_Spain","name":"Puerto España","id":"America/Port_of_Spain"},{"value":"America/Porto_Velho","name":"Porto Velho","id":"America/Porto_Velho"},{"value":"America/Puerto_Rico","name":"Puerto Rico","id":"America/Puerto_Rico"},{"value":"America/Punta_Arenas","name":"Punta Arenas","id":"America/Punta_Arenas"},{"value":"America/Rainy_River","name":"Rainy River","id":"America/Rainy_River"},{"value":"America/Rankin_Inlet","name":"Rankin Inlet","id":"America/Rankin_Inlet"},{"value":"America/Recife","name":"Recife","id":"America/Recife"},{"value":"America/Regina","name":"Regina","id":"America/Regina"},{"value":"America/Resolute","name":"Resolute","id":"America/Resolute"},{"value":"America/Rio_Branco","name":"Río Branco","id":"America/Rio_Branco"},{"value":"America/Santa_Isabel","name":"Santa Isabel","id":"America/Santa_Isabel"},{"value":"America/Santarem","name":"Santarém","id":"America/Santarem"},{"value":"America/Santiago","name":"Santiago de Chile","id":"America/Santiago"},{"value":"America/Santo_Domingo","name":"Santo Domingo","id":"America/Santo_Domingo"},{"value":"America/Sao_Paulo","name":"São Paulo","id":"America/Sao_Paulo"},{"value":"America/Scoresbysund","name":"Ittoqqortoormiit","id":"America/Scoresbysund"},{"value":"America/Sitka","name":"Sitka","id":"America/Sitka"},{"value":"America/St_Barthelemy","name":"San Bartolomé","id":"America/St_Barthelemy"},{"value":"America/St_Johns","name":"San Juan de Terranova","id":"America/St_Johns"},{"value":"America/St_Kitts","name":"San Cristóbal","id":"America/St_Kitts"},{"value":"America/St_Lucia","name":"Santa Lucía","id":"America/St_Lucia"},{"value":"America/St_Thomas","name":"St. Thomas","id":"America/St_Thomas"},{"value":"America/St_Vincent","name":"San Vicente","id":"America/St_Vincent"},{"value":"America/Swift_Current","name":"Swift Current","id":"America/Swift_Current"},{"value":"America/Tegucigalpa","name":"Tegucigalpa","id":"America/Tegucigalpa"},{"value":"America/Thule","name":"Thule","id":"America/Thule"},{"value":"America/Thunder_Bay","name":"Thunder Bay","id":"America/Thunder_Bay"},{"value":"America/Tijuana","name":"Tijuana","id":"America/Tijuana"},{"value":"America/Toronto","name":"Toronto","id":"America/Toronto"},{"value":"America/Tortola","name":"Tórtola","id":"America/Tortola"},{"value":"America/Vancouver","name":"Vancouver","id":"America/Vancouver"},{"value":"America/Whitehorse","name":"Whitehorse","id":"America/Whitehorse"},{"value":"America/Winnipeg","name":"Winnipeg","id":"America/Winnipeg"},{"value":"America/Yakutat","name":"Yakutat","id":"America/Yakutat"},{"value":"America/Yellowknife","name":"Yellowknife","id":"America/Yellowknife"},{"value":"Antarctica/Casey","name":"Casey","id":"Antarctica/Casey"},{"value":"Antarctica/Davis","name":"Davis","id":"Antarctica/Davis"},{"value":"Antarctica/DumontDUrville","name":"Dumont d’Urville","id":"Antarctica/DumontDUrville"},{"value":"Antarctica/Macquarie","name":"Macquarie","id":"Antarctica/Macquarie"},{"value":"Antarctica/Mawson","name":"Mawson","id":"Antarctica/Mawson"},{"value":"Antarctica/McMurdo","name":"McMurdo","id":"Antarctica/McMurdo"},{"value":"Antarctica/Palmer","name":"Palmer","id":"Antarctica/Palmer"},{"value":"Antarctica/Rothera","name":"Rothera","id":"Antarctica/Rothera"},{"value":"Antarctica/Syowa","name":"Syowa","id":"Antarctica/Syowa"},{"value":"Antarctica/Troll","name":"Troll","id":"Antarctica/Troll"},{"value":"Antarctica/Vostok","name":"Vostok","id":"Antarctica/Vostok"},{"value":"Arctic/Longyearbyen","name":"Longyearbyen","id":"Arctic/Longyearbyen"},{"value":"Asia/Aden","name":"Adén","id":"Asia/Aden"},{"value":"Asia/Almaty","name":"Almaty","id":"Asia/Almaty"},{"value":"Asia/Amman","name":"Ammán","id":"Asia/Amman"},{"value":"Asia/Anadyr","name":"Anádyr","id":"Asia/Anadyr"},{"value":"Asia/Aqtau","name":"Aktau","id":"Asia/Aqtau"},{"value":"Asia/Aqtobe","name":"Aktobe","id":"Asia/Aqtobe"},{"value":"Asia/Ashgabat","name":"Asjabad","id":"Asia/Ashgabat"},{"value":"Asia/Atyrau","name":"Atyrau","id":"Asia/Atyrau"},{"value":"Asia/Baghdad","name":"Bagdad","id":"Asia/Baghdad"},{"value":"Asia/Bahrain","name":"Baréin","id":"Asia/Bahrain"},{"value":"Asia/Baku","name":"Bakú","id":"Asia/Baku"},{"value":"Asia/Bangkok","name":"Bangkok","id":"Asia/Bangkok"},{"value":"Asia/Barnaul","name":"Barnaúl","id":"Asia/Barnaul"},{"value":"Asia/Beirut","name":"Beirut","id":"Asia/Beirut"},{"value":"Asia/Bishkek","name":"Bishkek","id":"Asia/Bishkek"},{"value":"Asia/Brunei","name":"Brunéi","id":"Asia/Brunei"},{"value":"Asia/Calcutta","name":"Calcuta","id":"Asia/Calcutta"},{"value":"Asia/Chita","name":"Chitá","id":"Asia/Chita"},{"value":"Asia/Choibalsan","name":"Choibalsan","id":"Asia/Choibalsan"},{"value":"Asia/Colombo","name":"Colombo","id":"Asia/Colombo"},{"value":"Asia/Damascus","name":"Damasco","id":"Asia/Damascus"},{"value":"Asia/Dhaka","name":"Daca","id":"Asia/Dhaka"},{"value":"Asia/Dili","name":"Dili","id":"Asia/Dili"},{"value":"Asia/Dubai","name":"Dubái","id":"Asia/Dubai"},{"value":"Asia/Dushanbe","name":"Dusambé","id":"Asia/Dushanbe"},{"value":"Asia/Famagusta","name":"Famagusta","id":"Asia/Famagusta"},{"value":"Asia/Gaza","name":"Gaza","id":"Asia/Gaza"},{"value":"Asia/Hebron","name":"Hebrón","id":"Asia/Hebron"},{"value":"Asia/Hong_Kong","name":"Hong Kong","id":"Asia/Hong_Kong"},{"value":"Asia/Hovd","name":"Hovd","id":"Asia/Hovd"},{"value":"Asia/Irkutsk","name":"Irkutsk","id":"Asia/Irkutsk"},{"value":"Asia/Jakarta","name":"Yakarta","id":"Asia/Jakarta"},{"value":"Asia/Jayapura","name":"Jayapura","id":"Asia/Jayapura"},{"value":"Asia/Jerusalem","name":"Jerusalén","id":"Asia/Jerusalem"},{"value":"Asia/Kabul","name":"Kabul","id":"Asia/Kabul"},{"value":"Asia/Kamchatka","name":"Kamchatka","id":"Asia/Kamchatka"},{"value":"Asia/Karachi","name":"Karachi","id":"Asia/Karachi"},{"value":"Asia/Katmandu","name":"Katmandú","id":"Asia/Katmandu"},{"value":"Asia/Khandyga","name":"Khandyga","id":"Asia/Khandyga"},{"value":"Asia/Krasnoyarsk","name":"Krasnoyarsk","id":"Asia/Krasnoyarsk"},{"value":"Asia/Kuala_Lumpur","name":"Kuala Lumpur","id":"Asia/Kuala_Lumpur"},{"value":"Asia/Kuching","name":"Kuching","id":"Asia/Kuching"},{"value":"Asia/Kuwait","name":"Kuwait","id":"Asia/Kuwait"},{"value":"Asia/Macau","name":"Macao","id":"Asia/Macau"},{"value":"Asia/Magadan","name":"Magadán","id":"Asia/Magadan"},{"value":"Asia/Makassar","name":"Makasar","id":"Asia/Makassar"},{"value":"Asia/Manila","name":"Manila","id":"Asia/Manila"},{"value":"Asia/Muscat","name":"Mascate","id":"Asia/Muscat"},{"value":"Asia/Nicosia","name":"Nicosia","id":"Asia/Nicosia"},{"value":"Asia/Novokuznetsk","name":"Novokuznetsk","id":"Asia/Novokuznetsk"},{"value":"Asia/Novosibirsk","name":"Novosibirsk","id":"Asia/Novosibirsk"},{"value":"Asia/Omsk","name":"Omsk","id":"Asia/Omsk"},{"value":"Asia/Oral","name":"Oral","id":"Asia/Oral"},{"value":"Asia/Phnom_Penh","name":"Phnom Penh","id":"Asia/Phnom_Penh"},{"value":"Asia/Pontianak","name":"Pontianak","id":"Asia/Pontianak"},{"value":"Asia/Pyongyang","name":"Pyongyang","id":"Asia/Pyongyang"},{"value":"Asia/Qatar","name":"Catar","id":"Asia/Qatar"},{"value":"Asia/Qyzylorda","name":"Kyzylorda","id":"Asia/Qyzylorda"},{"value":"Asia/Rangoon","name":"Yangón (Rangún)","id":"Asia/Rangoon"},{"value":"Asia/Riyadh","name":"Riad","id":"Asia/Riyadh"},{"value":"Asia/Saigon","name":"Ciudad Ho Chi Minh","id":"Asia/Saigon"},{"value":"Asia/Sakhalin","name":"Sajalín","id":"Asia/Sakhalin"},{"value":"Asia/Samarkand","name":"Samarcanda","id":"Asia/Samarkand"},{"value":"Asia/Seoul","name":"Seúl","id":"Asia/Seoul"},{"value":"Asia/Shanghai","name":"Shanghái","id":"Asia/Shanghai"},{"value":"Asia/Singapore","name":"Singapur","id":"Asia/Singapore"},{"value":"Asia/Srednekolymsk","name":"Srednekolimsk","id":"Asia/Srednekolymsk"},{"value":"Asia/Taipei","name":"Taipéi","id":"Asia/Taipei"},{"value":"Asia/Tashkent","name":"Taskent","id":"Asia/Tashkent"},{"value":"Asia/Tbilisi","name":"Tiflis","id":"Asia/Tbilisi"},{"value":"Asia/Tehran","name":"Teherán","id":"Asia/Tehran"},{"value":"Asia/Thimphu","name":"Timbu","id":"Asia/Thimphu"},{"value":"Asia/Tokyo","name":"Tokio","id":"Asia/Tokyo"},{"value":"Asia/Tomsk","name":"Tomsk","id":"Asia/Tomsk"},{"value":"Asia/Ulaanbaatar","name":"Ulán Bator","id":"Asia/Ulaanbaatar"},{"value":"Asia/Urumqi","name":"Ürümqi","id":"Asia/Urumqi"},{"value":"Asia/Ust-Nera","name":"Ust-Nera","id":"Asia/Ust-Nera"},{"value":"Asia/Vientiane","name":"Vientián","id":"Asia/Vientiane"},{"value":"Asia/Vladivostok","name":"Vladivostok","id":"Asia/Vladivostok"},{"value":"Asia/Yakutsk","name":"Yakutsk","id":"Asia/Yakutsk"},{"value":"Asia/Yekaterinburg","name":"Ekaterimburgo","id":"Asia/Yekaterinburg"},{"value":"Asia/Yerevan","name":"Ereván","id":"Asia/Yerevan"},{"value":"Atlantic/Azores","name":"Azores","id":"Atlantic/Azores"},{"value":"Atlantic/Bermuda","name":"Bermudas","id":"Atlantic/Bermuda"},{"value":"Atlantic/Canary","name":"Islas Canarias","id":"Atlantic/Canary"},{"value":"Atlantic/Cape_Verde","name":"Cabo Verde","id":"Atlantic/Cape_Verde"},{"value":"Atlantic/Faeroe","name":"Islas Feroe","id":"Atlantic/Faeroe"},{"value":"Atlantic/Madeira","name":"Madeira","id":"Atlantic/Madeira"},{"value":"Atlantic/Reykjavik","name":"Reikiavik","id":"Atlantic/Reykjavik"},{"value":"Atlantic/South_Georgia","name":"Georgia del Sur","id":"Atlantic/South_Georgia"},{"value":"Atlantic/St_Helena","name":"Santa Elena","id":"Atlantic/St_Helena"},{"value":"Atlantic/Stanley","name":"Stanley","id":"Atlantic/Stanley"},{"value":"Australia/Adelaide","name":"Adelaida","id":"Australia/Adelaide"},{"value":"Australia/Brisbane","name":"Brisbane","id":"Australia/Brisbane"},{"value":"Australia/Broken_Hill","name":"Broken Hill","id":"Australia/Broken_Hill"},{"value":"Australia/Currie","name":"Currie","id":"Australia/Currie"},{"value":"Australia/Darwin","name":"Darwin","id":"Australia/Darwin"},{"value":"Australia/Eucla","name":"Eucla","id":"Australia/Eucla"},{"value":"Australia/Hobart","name":"Hobart","id":"Australia/Hobart"},{"value":"Australia/Lindeman","name":"Lindeman","id":"Australia/Lindeman"},{"value":"Australia/Lord_Howe","name":"Lord Howe","id":"Australia/Lord_Howe"},{"value":"Australia/Melbourne","name":"Melbourne","id":"Australia/Melbourne"},{"value":"Australia/Perth","name":"Perth","id":"Australia/Perth"},{"value":"Australia/Sydney","name":"Sídney","id":"Australia/Sydney"},{"value":"Etc/UTC","name":"tiempo universal coordinado","id":"Etc/UTC"},{"value":"Europe/Amsterdam","name":"Ámsterdam","id":"Europe/Amsterdam"},{"value":"Europe/Andorra","name":"Andorra","id":"Europe/Andorra"},{"value":"Europe/Astrakhan","name":"Astracán","id":"Europe/Astrakhan"},{"value":"Europe/Athens","name":"Atenas","id":"Europe/Athens"},{"value":"Europe/Belgrade","name":"Belgrado","id":"Europe/Belgrade"},{"value":"Europe/Berlin","name":"Berlín","id":"Europe/Berlin"},{"value":"Europe/Bratislava","name":"Bratislava","id":"Europe/Bratislava"},{"value":"Europe/Brussels","name":"Bruselas","id":"Europe/Brussels"},{"value":"Europe/Bucharest","name":"Bucarest","id":"Europe/Bucharest"},{"value":"Europe/Budapest","name":"Budapest","id":"Europe/Budapest"},{"value":"Europe/Busingen","name":"Busingen","id":"Europe/Busingen"},{"value":"Europe/Chisinau","name":"Chisináu","id":"Europe/Chisinau"},{"value":"Europe/Copenhagen","name":"Copenhague","id":"Europe/Copenhagen"},{"value":"Europe/Dublin","name":"hora de verano de IrlandaDublín","id":"Europe/Dublin"},{"value":"Europe/Gibraltar","name":"Gibraltar","id":"Europe/Gibraltar"},{"value":"Europe/Guernsey","name":"Guernsey","id":"Europe/Guernsey"},{"value":"Europe/Helsinki","name":"Helsinki","id":"Europe/Helsinki"},{"value":"Europe/Isle_of_Man","name":"Isla de Man","id":"Europe/Isle_of_Man"},{"value":"Europe/Istanbul","name":"Estambul","id":"Europe/Istanbul"},{"value":"Europe/Jersey","name":"Jersey","id":"Europe/Jersey"},{"value":"Europe/Kaliningrad","name":"Kaliningrado","id":"Europe/Kaliningrad"},{"value":"Europe/Kiev","name":"Kiev","id":"Europe/Kiev"},{"value":"Europe/Kirov","name":"Kírov","id":"Europe/Kirov"},{"value":"Europe/Lisbon","name":"Lisboa","id":"Europe/Lisbon"},{"value":"Europe/Ljubljana","name":"Liubliana","id":"Europe/Ljubljana"},{"value":"Europe/London","name":"hora de verano británicaLondres","id":"Europe/London"},{"value":"Europe/Luxembourg","name":"Luxemburgo","id":"Europe/Luxembourg"},{"value":"Europe/Madrid","name":"Madrid","id":"Europe/Madrid"},{"value":"Europe/Malta","name":"Malta","id":"Europe/Malta"},{"value":"Europe/Mariehamn","name":"Mariehamn","id":"Europe/Mariehamn"},{"value":"Europe/Minsk","name":"Minsk","id":"Europe/Minsk"},{"value":"Europe/Monaco","name":"Mónaco","id":"Europe/Monaco"},{"value":"Europe/Moscow","name":"Moscú","id":"Europe/Moscow"},{"value":"Europe/Oslo","name":"Oslo","id":"Europe/Oslo"},{"value":"Europe/Paris","name":"París","id":"Europe/Paris"},{"value":"Europe/Podgorica","name":"Podgorica","id":"Europe/Podgorica"},{"value":"Europe/Prague","name":"Praga","id":"Europe/Prague"},{"value":"Europe/Riga","name":"Riga","id":"Europe/Riga"},{"value":"Europe/Rome","name":"Roma","id":"Europe/Rome"},{"value":"Europe/Samara","name":"Samara","id":"Europe/Samara"},{"value":"Europe/San_Marino","name":"San Marino","id":"Europe/San_Marino"},{"value":"Europe/Sarajevo","name":"Sarajevo","id":"Europe/Sarajevo"},{"value":"Europe/Saratov","name":"Sarátov","id":"Europe/Saratov"},{"value":"Europe/Simferopol","name":"Simferópol","id":"Europe/Simferopol"},{"value":"Europe/Skopje","name":"Skopie","id":"Europe/Skopje"},{"value":"Europe/Sofia","name":"Sofía","id":"Europe/Sofia"},{"value":"Europe/Stockholm","name":"Estocolmo","id":"Europe/Stockholm"},{"value":"Europe/Tallinn","name":"Tallin","id":"Europe/Tallinn"},{"value":"Europe/Tirane","name":"Tirana","id":"Europe/Tirane"},{"value":"Europe/Ulyanovsk","name":"Uliánovsk","id":"Europe/Ulyanovsk"},{"value":"Europe/Uzhgorod","name":"Úzhgorod","id":"Europe/Uzhgorod"},{"value":"Europe/Vaduz","name":"Vaduz","id":"Europe/Vaduz"},{"value":"Europe/Vatican","name":"El Vaticano","id":"Europe/Vatican"},{"value":"Europe/Vienna","name":"Viena","id":"Europe/Vienna"},{"value":"Europe/Vilnius","name":"Vilna","id":"Europe/Vilnius"},{"value":"Europe/Volgograd","name":"Volgogrado","id":"Europe/Volgograd"},{"value":"Europe/Warsaw","name":"Varsovia","id":"Europe/Warsaw"},{"value":"Europe/Zagreb","name":"Zagreb","id":"Europe/Zagreb"},{"value":"Europe/Zaporozhye","name":"Zaporiyia","id":"Europe/Zaporozhye"},{"value":"Europe/Zurich","name":"Zúrich","id":"Europe/Zurich"},{"value":"Indian/Antananarivo","name":"Antananarivo","id":"Indian/Antananarivo"},{"value":"Indian/Chagos","name":"Chagos","id":"Indian/Chagos"},{"value":"Indian/Christmas","name":"Navidad","id":"Indian/Christmas"},{"value":"Indian/Cocos","name":"Cocos","id":"Indian/Cocos"},{"value":"Indian/Comoro","name":"Comoras","id":"Indian/Comoro"},{"value":"Indian/Kerguelen","name":"Kerguelen","id":"Indian/Kerguelen"},{"value":"Indian/Mahe","name":"Mahé","id":"Indian/Mahe"},{"value":"Indian/Maldives","name":"Maldivas","id":"Indian/Maldives"},{"value":"Indian/Mauritius","name":"Mauricio","id":"Indian/Mauritius"},{"value":"Indian/Mayotte","name":"Mayotte","id":"Indian/Mayotte"},{"value":"Indian/Reunion","name":"Reunión","id":"Indian/Reunion"},{"value":"Pacific/Apia","name":"Apia","id":"Pacific/Apia"},{"value":"Pacific/Auckland","name":"Auckland","id":"Pacific/Auckland"},{"value":"Pacific/Bougainville","name":"Bougainville","id":"Pacific/Bougainville"},{"value":"Pacific/Chatham","name":"Chatham","id":"Pacific/Chatham"},{"value":"Pacific/Easter","name":"Isla de Pascua","id":"Pacific/Easter"},{"value":"Pacific/Efate","name":"Efate","id":"Pacific/Efate"},{"value":"Pacific/Enderbury","name":"Enderbury","id":"Pacific/Enderbury"},{"value":"Pacific/Fakaofo","name":"Fakaofo","id":"Pacific/Fakaofo"},{"value":"Pacific/Fiji","name":"Fiyi","id":"Pacific/Fiji"},{"value":"Pacific/Funafuti","name":"Funafuti","id":"Pacific/Funafuti"},{"value":"Pacific/Galapagos","name":"Galápagos","id":"Pacific/Galapagos"},{"value":"Pacific/Gambier","name":"Gambier","id":"Pacific/Gambier"},{"value":"Pacific/Guadalcanal","name":"Guadalcanal","id":"Pacific/Guadalcanal"},{"value":"Pacific/Guam","name":"Guam","id":"Pacific/Guam"},{"value":"Pacific/Honolulu","name":"Honolulú","id":"Pacific/Honolulu"},{"value":"Pacific/Johnston","name":"Johnston","id":"Pacific/Johnston"},{"value":"Pacific/Kiritimati","name":"Kiritimati","id":"Pacific/Kiritimati"},{"value":"Pacific/Kosrae","name":"Kosrae","id":"Pacific/Kosrae"},{"value":"Pacific/Kwajalein","name":"Kwajalein","id":"Pacific/Kwajalein"},{"value":"Pacific/Majuro","name":"Majuro","id":"Pacific/Majuro"},{"value":"Pacific/Marquesas","name":"Marquesas","id":"Pacific/Marquesas"},{"value":"Pacific/Midway","name":"Midway","id":"Pacific/Midway"},{"value":"Pacific/Nauru","name":"Nauru","id":"Pacific/Nauru"},{"value":"Pacific/Niue","name":"Niue","id":"Pacific/Niue"},{"value":"Pacific/Norfolk","name":"Norfolk","id":"Pacific/Norfolk"},{"value":"Pacific/Noumea","name":"Numea","id":"Pacific/Noumea"},{"value":"Pacific/Pago_Pago","name":"Pago Pago","id":"Pacific/Pago_Pago"},{"value":"Pacific/Palau","name":"Palaos","id":"Pacific/Palau"},{"value":"Pacific/Pitcairn","name":"Pitcairn","id":"Pacific/Pitcairn"},{"value":"Pacific/Ponape","name":"Pohnpei","id":"Pacific/Ponape"},{"value":"Pacific/Port_Moresby","name":"Port Moresby","id":"Pacific/Port_Moresby"},{"value":"Pacific/Rarotonga","name":"Rarotonga","id":"Pacific/Rarotonga"},{"value":"Pacific/Saipan","name":"Saipán","id":"Pacific/Saipan"},{"value":"Pacific/Tahiti","name":"Tahití","id":"Pacific/Tahiti"},{"value":"Pacific/Tarawa","name":"Tarawa","id":"Pacific/Tarawa"},{"value":"Pacific/Tongatapu","name":"Tongatapu","id":"Pacific/Tongatapu"},{"value":"Pacific/Truk","name":"Chuuk","id":"Pacific/Truk"},{"value":"Pacific/Wake","name":"Wake","id":"Pacific/Wake"},{"value":"Pacific/Wallis","name":"Wallis","id":"Pacific/Wallis"}];
};

return moment;
})));

moment.fn.shortDateNoYear = function(){ return this.format('D MMM'); };
moment.fn.shortDate = function(){ return this.format('D MMM, YYYY'); };
moment.fn.longDate = function(){ return this.format('MMMM D, YYYY h:mma'); };
moment.fn.relativeAge = function(opts){ return Discourse.Formatter.relativeAge(this.toDate(), opts)};
