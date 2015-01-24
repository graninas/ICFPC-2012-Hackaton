var
    nc = require('ncurses'),
    file = require('read-file'),
    yargs = require('yargs');

var
    index = 0,
    slides = [];

var args = yargs
    .demand(['input'])
    .argv;

var entry = null;

file.readFileSync(args.input).split('\n').forEach(function(line) {
    var tag = /^\s*>\s*(.*)$/img.exec(line);

    if (tag !== null) {
        if (entry) slides.push(entry);

        entry = {
            move: tag[1],
            lines: []
        };
    } else {
        if (entry) {
            entry.lines.push(line);
        }
    }
});

if (entry) {
    slides.push(entry);
}

var w = new nc.Window();

w.on('inputChar', function(symbol, code) {
    switch (code) {
        case nc.keys.NEWLINE:
            w.close();
            break;

        case nc.keys.LEFT:
            index = index - 1;
            if (index < 0) index = slides.length - 1;

            drawSlide();
            break;

        case nc.keys.RIGHT:
            index = index + 1;
            if (index >= slides.length) index = 0;

            drawSlide();
            break;
    }
});

drawSlide();

function drawSlide() {
    var slide = slides[index];

    w.clear();

    w.insstr(0, 0, 'Step: ' + (index + 1) + '/' + slides.length);
    w.insstr(1, 0, 'Move: ' + slide.move);

    slide.lines.forEach(function(line, n) {
        w.insstr(2 + n, 0, line);
    });

    w.refresh();
}
