const gulp = require("gulp");
const jade = require("gulp-jade");
const minifyCss = require("gulp-minify-css");
const gulpWebpack = require("gulp-webpack");
const webpack = require("webpack");

let dist = "../dist";

gulp.task("templates", () => {
    return gulp.src("./app/templates/**/*.jade")
	.pipe(jade())
	.pipe(gulp.dest(`${dist}/templates`));
});

gulp.task("css", () => {
    return gulp.src("./app/css/*.css")
	.pipe(minifyCss())
	.pipe(gulp.dest(`${dist}/css`));
});

gulp.task("css-vendor", () => {
    return gulp.src("./app/css/vendor/**/*")
        .pipe(gulp.dest(`${dist}/css/vendor`));
});

gulp.task("bootstrap", () => {
    return gulp.src("node_modules/bootstrap/dist/css/*.css")
	.pipe(gulp.dest(`${dist}/css/vendor`));
});

gulp.task("js-vendor", () => {
    return gulp.src("./app/js/vendor/**/*")
        .pipe(gulp.dest(`${dist}/js/vendor`));
});

gulp.task("img", () => {
    return gulp.src("./app/img/*")
    .pipe(gulp.dest(`${dist}/img`));
});

gulp.task("favicon", () => {
    return gulp.src("./app/favicon/*")
	.pipe(gulp.dest(`${dist}/`));
});

exports.default = gulp.task("default", gulp.parallel("templates", "css", "css-vendor", "bootstrap", "js-vendor", "img", "favicon"));
