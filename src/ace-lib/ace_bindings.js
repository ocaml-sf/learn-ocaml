/* TryOCaml for the FUN platform
 * Copyright (C) 2015 OCamlPro: Grégoire Henry, Çağdaş Bozman.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

var define_ocaml_mode = function (name, helpers) {

ace.define(name,["require","exports","module",
                 "ace/lib/oop","ace/mode/text",
                 "ace/mode/text_highlight_rules"],
function(require, exports, module) {
"use strict";

var check_state = function (state) {
    // console.log(state);
    if (state == undefined || state == "start") {
        return (helpers.initialState ());
    } else {
        return state;
    }
};

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var Mode = function() {
    this.HighlightRules = TextHighlightRules;
    this.$tokenizer = {
        getLineTokens: function (line, state, row, doc) {
            return helpers.getLineTokens(line, check_state(state), row, doc);
        },
    };
};
oop.inherits(Mode, TextMode);

(function() {

    this.getNextLineIndent = function(state, line, tab) {
        return helpers.getNextLineIndent(check_state(state), line, tab);
    };

    if (helpers.checkOutdent) {
        this.checkOutdent = function(state, line, input) {
            return helpers.checkOutdent(check_state(state), line, input);
        };
    };

    if (helpers.autoOutdent) {
        this.autoOutdent = function(state, doc, row) {
            return helpers.autoOutdent(check_state(state), doc, row);
        };
    };

    this.$id = name;

}).call(Mode.prototype);

exports.Mode = Mode;

});

};
