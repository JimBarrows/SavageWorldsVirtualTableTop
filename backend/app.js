var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var sequelize = require('sequelize');

var routes = require('./routes/index');
var users = require('./routes/users');

var db = new sequelize('database', 'username', 'password', {
  host: 'localhost',
  //dialect: 'mysql'|'mariadb'|'sqlite'|'postgres'|'mssql',
  dialect: 'sqlite',

  pool: {
    max: 5,
    min: 0,
    idle: 10000
  },

  // SQLite only
  storage: '../temp/database.sqlite'
});

var StandardHindrances = require('./routes/standard-hindrance')(db);
var StandardEdges = require('./routes/standard-edges')(db);
var StandardPowers = require('./routes/standard-powers')(db);
var StandardSkills = require('./routes/standard-skills')(db);
var StandardPowerTrappings = require('./routes/standard-power-trappings')(db);
var StandardGear = require('./routes/standard-gear')(db);
var SkillDescriptions = require('./routes/skill-descriptions')(db);
var Hindrances = require('./routes/hindrances')(db);
var Edges = require('./routes/edges')(db);
var PlotPoints = require('./routes/plot-points')(db);

db.sync();

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', routes);
app.use('/users', users);
app.use('/api/standardHindrances', StandardHindrances);
app.use('/api/standardEdges', StandardEdges);
app.use('/api/standardPowers', StandardPowers);
app.use('/api/standardSkillDescriptions', StandardSkills);
app.use('/api/standardPowerTrappings', StandardPowerTrappings);
app.use('/api/standardGears', StandardGear);
app.use('/api/plotPoints', PlotPoints);
app.use('/api/skillDescriptions', SkillDescriptions);
app.use('/api/hindrances', Hindrances);
app.use('/api/edges', Edges);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
  app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
      message: err.message,
      error: err
    });
  });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
  res.status(err.status || 500);
  res.render('error', {
    message: err.message,
    error: {}
  });
});


module.exports = app;
