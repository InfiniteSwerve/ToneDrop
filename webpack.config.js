// webpack.config.js
const path = require('path');

module.exports = {
  resolve: {
    modules: [path.resolve(__dirname, 'node_modules'), 'node_modules'],
    extensions: ['.js', '.jsx', '.json', '.wasm']
  },
};
