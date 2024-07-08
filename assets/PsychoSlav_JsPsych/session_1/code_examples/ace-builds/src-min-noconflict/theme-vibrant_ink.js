ace.define("ace/theme/vibrant_ink.css",["require","exports","module"],function(e,t,n){n.exports=".ace-vibrant-ink .ace_gutter {\n  background: #1a1a1a;\n  color: #BEBEBE\n}\n\n.ace-vibrant-ink .ace_print-margin {\n  width: 1px;\n  background: #1a1a1a\n}\n\n.ace-vibrant-ink {\n  background-color: #0F0F0F;\n  color: #FFFFFF\n}\n\n.ace-vibrant-ink .ace_cursor {\n  color: #FFFFFF\n}\n\n.ace-vibrant-ink .ace_marker-layer .ace_selection {\n  background: #6699CC\n}\n\n.ace-vibrant-ink.ace_multiselect .ace_selection.ace_start {\n  box-shadow: 0 0 3px 0px #0F0F0F;\n}\n\n.ace-vibrant-ink .ace_marker-layer .ace_step {\n  background: rgb(102, 82, 0)\n}\n\n.ace-vibrant-ink .ace_marker-layer .ace_bracket {\n  margin: -1px 0 0 -1px;\n  border: 1px solid #404040\n}\n\n.ace-vibrant-ink .ace_marker-layer .ace_active-line {\n  background: #333333\n}\n\n.ace-vibrant-ink .ace_gutter-active-line {\n  background-color: #333333\n}\n\n.ace-vibrant-ink .ace_marker-layer .ace_selected-word {\n  border: 1px solid #6699CC\n}\n\n.ace-vibrant-ink .ace_invisible {\n  color: #404040\n}\n\n.ace-vibrant-ink .ace_keyword,\n.ace-vibrant-ink .ace_meta {\n  color: #FF6600\n}\n\n.ace-vibrant-ink .ace_constant,\n.ace-vibrant-ink .ace_constant.ace_character,\n.ace-vibrant-ink .ace_constant.ace_character.ace_escape,\n.ace-vibrant-ink .ace_constant.ace_other {\n  color: #339999\n}\n\n.ace-vibrant-ink .ace_constant.ace_numeric {\n  color: #99CC99\n}\n\n.ace-vibrant-ink .ace_invalid,\n.ace-vibrant-ink .ace_invalid.ace_deprecated {\n  color: #CCFF33;\n  background-color: #000000\n}\n\n.ace-vibrant-ink .ace_fold {\n  background-color: #FFCC00;\n  border-color: #FFFFFF\n}\n\n.ace-vibrant-ink .ace_entity.ace_name.ace_function,\n.ace-vibrant-ink .ace_support.ace_function,\n.ace-vibrant-ink .ace_variable {\n  color: #FFCC00\n}\n\n.ace-vibrant-ink .ace_variable.ace_parameter {\n  font-style: italic\n}\n\n.ace-vibrant-ink .ace_string {\n  color: #66FF00\n}\n\n.ace-vibrant-ink .ace_string.ace_regexp {\n  color: #44B4CC\n}\n\n.ace-vibrant-ink .ace_comment {\n  color: #9933CC\n}\n\n.ace-vibrant-ink .ace_entity.ace_other.ace_attribute-name {\n  font-style: italic;\n  color: #99CC99\n}\n\n.ace-vibrant-ink .ace_indent-guide {\n  background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAACCAYAAACZgbYnAAAAEklEQVQImWNgYGBgYNDTc/oPAALPAZ7hxlbYAAAAAElFTkSuQmCC) right repeat-y\n}\n\n.ace-vibrant-ink .ace_indent-guide-active {\n  background: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAACCAYAAACZgbYnAAAAEklEQVQIW2PQ1dX9zzBz5sz/ABCcBFFentLlAAAAAElFTkSuQmCC) right repeat-y;\n}\n"}),ace.define("ace/theme/vibrant_ink",["require","exports","module","ace/theme/vibrant_ink.css","ace/lib/dom"],function(e,t,n){t.isDark=!0,t.cssClass="ace-vibrant-ink",t.cssText=e("./vibrant_ink.css");var r=e("../lib/dom");r.importCssString(t.cssText,t.cssClass,!1)});                (function() {
                    ace.require(["ace/theme/vibrant_ink"], function(m) {
                        if (typeof module == "object" && typeof exports == "object" && module) {
                            module.exports = m;
                        }
                    });
                })();
            