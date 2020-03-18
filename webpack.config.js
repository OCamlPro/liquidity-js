// const TerserPlugin = require('terser-webpack-plugin');

module.exports = {
    node: {
        fs: 'empty',
        child_process: 'empty',
    },
    output: {
        library: 'liquidity',
        libraryTarget: 'umd',
    },
    // optimization: {
    //     minimizer: [
    //         new TerserPlugin({
    //             // sourceMap: true,
    //             // Must be set to true if using source-maps in production
    //             terserOptions: {
    //                 compress: {
    //                     drop_console: true,
    //                 },
    //             },
    //         }),
    //     ],
    // },
};
