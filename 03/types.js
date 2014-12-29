var HTML = (function () {
    function HTML(html) {
        this.html = html;
    }
    return HTML;
})();

function escape(str) {
    var escaped = str.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&apos;");
    return new HTML(escaped);
}

function insert(html) {
    var target = document.getElementById('target');
    target.innerHTML = html.html;
}

function display() {
    var text = document.getElementsByTagName('input')[0];
    insert(escape(text.value));
}
