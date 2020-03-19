
function save_properties(obj, props) {
    var saved = {};
    props.forEach(key => {
        if (obj.hasOwnProperty(key)) { saved[key] = obj[key] }
    });
    return saved;
}

function restore_properties(obj, props, saved) {
    props.forEach(key => {
        if (saved.hasOwnProperty(key)) {
            obj[key] = saved[key]
        } else {
            delete obj[key]
        }
    });
}

exports.is_ready = false;
exports.ready = new Promise(resolve => {
    const _sodium = require("libsodium-wrappers-sumo");
    _sodium.ready.then(() => {
        var saved = save_properties(global, ['sodium', 'blakejs']);
        global.sodium = _sodium;
        global.blakejs = require("blakejs");
        if (typeof module !== 'undefined' && module.exports) {
            global.XMLHttpRequest = require("xmlhttprequest").XMLHttpRequest;
        }
        const liquidity = require("../_obuild/liquidity-js/liquidity-js.js");
        Object.assign(exports, liquidity);
        exports.is_ready = true;
        resolve();
        restore_properties(global, ['sodium', 'blakejs'], saved);
    })
})
