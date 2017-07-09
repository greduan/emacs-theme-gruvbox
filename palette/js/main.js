var palette = {
  "dark0_hard":            "#1d2021", "dark0_hard-xterm":      "#1c1c1c",
  "dark0":                 "#282828", "dark0-xterm":           "#262626",
  "dark0_soft":            "#32302f", "dark0_soft-xterm":      "#303030",
  "dark1":                 "#3c3836", "dark1-xterm":           "#3a3a3a",
  "dark2":                 "#504945", "dark2-xterm":           "#4e4e4e",
  "dark3":                 "#665c54", "dark3-xterm":           "#626262",
  "dark4":                 "#7c6f64", "dark4-xterm":           "#767676",
  "gray":                  "#928374", "gray-xterm":            "#8a8a8a",
  "light0_hard":           "#ffffc8", "light0_hard-xterm":     "#ffffd7",
  "light0":                "#fdf4c1", "light0-xterm":          "#ffffaf",
  "light0_soft":           "#f4e8ba", "light0_soft-xterm":     "#ffdfaf",
  "light1":                "#ebdbb2", "light1-xterm":          "#ffdfaf",
  "light2":                "#d5c4a1", "light2-xterm":          "#bcbcbc",
  "light3":                "#bdae93", "light3-xterm":          "#a8a8a8",
  "light4":                "#a89984", "light4-xterm":          "#949494",
  "bright_red":            "#fb4933", "bright_red-xterm":      "#d75f5f",
  "bright_green":          "#b8bb26", "bright_green-xterm":    "#afaf00",
  "bright_yellow":         "#fabd2f", "bright_yellow-xterm":   "#ffaf00",
  "bright_blue":           "#83a598", "bright_blue-xterm":     "#87afaf",
  "bright_purple":         "#d3869b", "bright_purple-xterm":   "#d787af",
  "bright_aqua":           "#8ec07c", "bright_aqua-xterm":     "#87af87",
  "bright_orange":         "#fe8019", "bright_orange-xterm":   "#ff8700",
  "neutral_red":           "#fb4934", "neutral_red-xterm":     "#d75f5f",
  "neutral_green":         "#b8bb26", "neutral_green-xterm":   "#afaf00",
  "neutral_yellow":        "#fabd2f", "neutral_yellow-xterm":  "#ffaf00",
  "neutral_blue":          "#83a598", "neutral_blue-xterm":    "#87afaf",
  "neutral_purple":        "#d3869b", "neutral_purple-xterm":  "#d787af",
  "neutral_aqua":          "#8ec07c", "neutral_aqua-xterm":    "#87af87",
  "neutral_orange":        "#fe8019", "neutral_orange-xterm":  "#ff8700",
  "faded_red":             "#9d0006", "faded_red-xterm":       "#870000",
  "faded_green":           "#79740e", "faded_green-xterm":     "#878700",
  "faded_yellow":          "#b57614", "faded_yellow-xterm":    "#af8700",
  "faded_blue":            "#076678", "faded_blue-xterm":      "#005f87",
  "faded_purple":          "#8f3f71", "faded_purple-xterm":    "#875f87",
  "faded_aqua":            "#427b58", "faded_aqua-xterm":      "#5f8787",
  "faded_orange":          "#af3a03", "faded_orange-xterm":    "#af5f00",
  "dark_red":              "#421E1E", "dark_red-xterm":        "#5f0000",
  "dark_blue":             "#2B3C44", "dark_blue-xterm":       "#000087",
  "dark_aqua":             "#36473A", "dark_aqua-xterm":       "#005f5f",
  "delimiter-one":         "#458588", "delimiter-one-xterm":   "#008787",
  "delimiter-two":         "#b16286", "delimiter-two-xterm":   "#d75f87",
  "delimiter-three":       "#8ec07c", "delimiter-three-xterm": "#87af87",
  "delimiter-four":        "#d65d0e", "delimiter-four-xterm":  "#d75f00",
  "white":                 "#FFFFFF", "white-xterm":           "#FFFFFF",
  "black":                 "#000000", "black-xterm":           "#000000",
  "sienna":                "#DD6F48", "sienna-xterm":          "#d7875f",
  "darkslategray4":        "#528B8B", "darkslategray4-xterm":  "#5f8787",
  "lightblue4":            "#66999D", "lightblue4-xterm":      "#5fafaf",
  "burlywood4":            "#BBAA97", "burlywood4-xterm":      "#afaf87",
  "aquamarine4":           "#83A598", "aquamarine4-xterm":     "#87af87",
  "turquoise4":            "#61ACBB", "turquoise4-xterm":      "#5fafaf"
};

window.onload=function(){
  var app = new Vue({
    el: "#app",
    data: {
      paletteGroups: [
        {
          title: "DARK TONES",
          prefix: "",
          colors: [
            "dark0_hard",
            "dark0",
            "dark0_soft",
            "dark1",
            "dark2",
            "dark3",
            "dark4"
          ]
        },
        {
          title: "MID TONES",
          prefix: "",
          colors: [
            "gray"
          ]
        },
        {
          title: "LIGHT TONES",
          prefix: "",
          colors: [
            "light0_hard",
            "light0",
            "light0_soft",
            "light1",
            "light2",
            "light3",
            "light4",
          ]
        },
        {
          title: "BRIGHT COLORS",
          prefix: "bright_",
          colors: [
            "bright_red",
            "bright_green",
            "bright_yellow",
            "bright_blue",
            "bright_purple",
            "bright_aqua",
            "bright_orange"
          ]
        },
        {
          title: "NEUTRAL COLORS",
          prefix: "neutral_",
          colors: [
            "neutral_red",
            "neutral_green",
            "neutral_yellow",
            "neutral_blue",
            "neutral_purple",
            "neutral_aqua",
            "neutral_orange"
          ]
        },
        {
          title: "FADED COLORS",
          prefix: "faded_",
          colors: [
            "faded_red",
            "faded_green",
            "faded_yellow",
            "faded_blue",
            "faded_purple",
            "faded_aqua",
            "faded_orange"
          ]
        },
        {
          title: "DARK NOTES",
          prefix: "dark_",
          colors: [
            "dark_red",
            "dark_blue",
            "dark_aqua",
          ]
        },
        {
          title:"SOLO NOTES",
          prefix: "",
          colors:[
            "sienna",
            "darkslategray4",
            "lightblue4",
            "burlywood4",
            "aquamarine4",
            "turquoise4"
          ]
        },
        {
          title: "RAINBOW DELIMITER COLORS",
          prefix: "delimiter-",
          colors: [
            "delimiter-one",
            "delimiter-two",
            "delimiter-three",
            "delimiter-four"
          ]
        }
      ]
    }
  });
};

Vue.component("palette-group", {
  props: ["title", "colors", "prefix"],
  template: `<div class="palette-group" v-on:click="makeImg">
               <h3>{{title}}</h3>
               <swatch v-for="swatch in colors"
                       v-bind:prefix="prefix"
                       v-bind:content="swatch">
               </swatch>
               <div class="separator"></div>
               <swatch v-for="swatch in colors"
                       is-xterm="true"
                       v-bind:prefix="prefix"
                       v-bind:content="swatch">
               </swatch>
               <div class="group-separator"></div>
             </div>
`,
  methods:{
    makeImg: function(e){
      html2canvas(e.target, {
        onrendered: function(canvas) {
          document.body.appendChild(canvas);
        },
        background: undefined
      });
    }
  }
});

Vue.component("swatch", {
  props: ["content", "isXterm", "prefix"],
  template: `<div class="gruvbox-swatch">
               <div v-bind:class="cssClass">
                 <div class="content">{{unprefixed}}</div>
                 <div class="color-label">{{color}}</div>
                 <div class="xterm" v-if="isXterm">Xterm 256</div>
               </div>
               <div v-bind:class="cssClassAfter"></div>
             </div>
`,
  computed: {
    cssClass: function(){
      return `gruvbox-${this.content}${this.isXterm == "true" ? "-xterm" : ""}`;
    },
    cssClassAfter: function(){
      return `${this.cssClass}-after`;
    },
    color: function(){
      return palette[`${this.content}${this.isXterm == "true" ? "-xterm" : ""}`];
    },
    unprefixed: function(){
      return this.content.replace(this.prefix, "");
    }
  }
});
