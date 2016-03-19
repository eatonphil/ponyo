import gulp from "gulp";
import jade from "gulp-jade";
import minifyCss from "gulp-minify-css";
import gulpWebpack from "gulp-webpack";
import webpack from "webpack";

let dist = "../dist";

gulp.task("templates", () => {
    gulp.src("./app/templates/**/*.jade")
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

gulp.task("default", ["templates", "css", "css-vendor", "bootstrap", "js-vendor", "img", "favicon"]);
