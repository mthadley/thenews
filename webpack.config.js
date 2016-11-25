module.exports = {
  entry: './src/index.js',

  output: {
    path: './dist',
    filename: 'index.js'
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm']
  },

  module: {
    loaders: [
      {
        test: /\.css$/,
        loaders: ['style', 'css', 'postcss']
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack?warn=1'
      }
    ],

    noParse: /\.elm$/,
  },

  postcss: function(webpack) {
    return [
      require("postcss-import")({ addDependencyTo: webpack }),
      require('postcss-cssnext')
    ];
  },

  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};
