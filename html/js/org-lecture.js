$( document ).ready(function() {
    $.urlParam = function(name){
        var results = new RegExp('[\?&]' + name + '=([^&#]*)').exec(window.location.href);
        if (results==null) {
            return null;
        }
        return decodeURI(results[1]) || 0;
    }

    if ($.urlParam('print') != null) {
        $(".slide").removeClass('w-100');
        var inner = $(".carousel-inner");
        inner.siblings().remove();
        inner.unwrap().unwrap().unwrap();
        inner.removeClass('carousel-inner');
        inner.children().children().unwrap();
        $(".slide-beamer").removeClass('d-block').removeClass('slide-beamer').addClass('slide-print');
    } else {
        if (window.location.href.match(/.*handout.html/)) {
            $('<strong> - Druckversion</strong>').appendTo('.subtitle');
        } else {
            var url = window.location.href.match(/.*\//) + "index.html";
            $('<a href="'+url+'">[Index]</a>').appendTo('#table-of-contents');
            var url = window.location.href.replace('.html', '.slides.pdf');
            $('<span>[PDF: </span>').appendTo('#table-of-contents');
            $('<a href="'+url+'">Folien,</a>').appendTo('#table-of-contents');
            var url = window.location.href.replace('.html', '.handout.pdf');
            $('<a href="'+url+'"> Handout</a>').appendTo('#table-of-contents');
            $('<span>]</span>').appendTo('#table-of-contents');
            var url = window.location.href.replace('.html', '.html?print=true');
            $('<a href="'+url+'">[Druckversion]</a>').appendTo('#table-of-contents');
        }

        $("h2, h3").each(function(idx, headline) {
            if (headline.id) {
                $('<a class="headlineref"  href="#'+headline.id+'">&#128279;</a>').appendTo(headline);
            }
        });

        $("div.carousel").each(function (idx, carousel) {
            $(carousel).find("div.carousel-inner").click(function(e) {
                if(e.shiftKey || e.ctrlKey || e.altKey) {
                    $(carousel).carousel('prev');
                } else {
                    $(carousel).carousel('next');
                }
                return true;
            });
        });

        $("pre.src-python, pre.src-javascript").each(function (idx, block) {
            var button = $('<button class="execute">Load Interpreter</button>');
            button.insertBefore(block);
            button.click(function(e) {
                // Click Klipse
                if ($("script#script-klipse").length == 0) {
                    var script = document.createElement('script');
                    script.id = 'script-klipse';
                    script.type='text/javascript';
                    script.src = "https://storage.googleapis.com/app.klipse.tech/plugin_prod/js/klipse_plugin.min.js";
                    script.onload = function() {
                        $("button.execute").detach();
                    }
                    document.head.appendChild(script);
                    var css = document.createElement('link');
                    css.rel = 'stylesheet'
                    css.type = 'text/css';
                    css.href = 'https://storage.googleapis.com/app.klipse.tech/css/codemirror.css';
                    script.id = 'script-klipse';
                    document.head.appendChild(css);
                }
            });
        });

        window.klipse_settings = {
            selector_eval_html: '.src-html',
            selector_eval_js: '.src-javascript',
            selector_eval_python_client: '.src-python',
            selector_eval_scheme: '.src-scheme',
            selector: '.src-clojure',
            selector_eval_ruby: '.src-ruby',
            eval_idle_msec: 100
        };

        requestAnimationFrame(function() {
            if (location.hash) {
                $(document).scrollTop( $(location.hash).offset().top );
            }
        });
    }
});
