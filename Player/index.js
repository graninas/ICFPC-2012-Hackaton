var nc = require('ncurses');

var
    index = 0,
    slides = [];

process.stdin.on('readable', function() {
    var chunk = process.stdin.read();

    if (chunk !== null) {
        console.log("234234" + chunk);
    }
});

process.stdin.on('end', function() {
    return;
    var w = new nc.Window();

    w.on('inputChar', function(symbol, code) {
        switch (code) {
            case 27:
                w.close();
                break;

            case 260:
                index = index - 1;
                if (index < 0) index = slides.length - 1;

                drawSlide();
                break;

            case 261:
                index = index + 1;
                if (index >= slides.length) index = 0;

                drawSlide();
                break;
        }
    });

    drawSlide();

    function drawSlide(index) {
        var slide = slides[index];

        w.clear();

        w.insstr(1, 1, 'Step: ' + (index + 1) + '/' + slides.length);
        w.insstr(1, 2, 'Move: ' + slide.move);

        w.refresh();
    }
});