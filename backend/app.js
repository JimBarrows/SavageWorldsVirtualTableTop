var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');

var environment = process.env.NODE_ENV || 'development'
var config = require('./config/config.json')[environment];

console.log('Build route objects');
var Chapters = require('./routes/chapters');
var Characters = require('./routes/characters');
var Edges = require('./routes/edges');
var Gear = require('./routes/gear');
var Hindrances = require('./routes/hindrances');
var PlotPoints = require('./routes/plot-points');
var Powers = require('./routes/powers');
var Races = require('./routes/races');
var RacialAbilities = require('./routes/racial-ability');
var Routes = require('./routes/index');
var Scenes = require('./routes/scenes');
var SkillDescriptions = require('./routes/skill-descriptions');
var Skills = require('./routes/skills');
var StandardEdges = require('./routes/standard-edges');
var StandardGear = require('./routes/standard-gear');
var StandardHindrances = require('./routes/standard-hindrance');
var StandardPowerTrappings = require('./routes/standard-power-trappings');
var StandardPowers = require('./routes/standard-powers');
var StandardSkills = require('./routes/standard-skills');
var Stories = require('./routes/stories');
var Users = require('./routes/users');

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

console.log("Building Routes");
app.use('/', Routes);
app.use('/api/chapters', Chapters);
app.use('/api/characters', Characters);
app.use('/api/edges', Edges);
app.use('/api/gears', Gear);
app.use('/api/hindrances', Hindrances);
app.use('/api/plotPoints', PlotPoints);
app.use('/api/powers', Powers);
app.use('/api/races', Races);
app.use('/api/racialAbilities', RacialAbilities);
app.use('/api/scenes', Scenes);
app.use('/api/skillDescriptions', SkillDescriptions);
app.use('/api/skills', Skills);
app.use('/api/standardEdges', StandardEdges);
app.use('/api/standardGears', StandardGear);
app.use('/api/standardHindrances', StandardHindrances);
app.use('/api/standardPowerTrappings', StandardPowerTrappings);
app.use('/api/standardPowers', StandardPowers);
app.use('/api/standardSkillDescriptions', StandardSkills);
app.use('/api/stories', Stories);
app.use('/users', Users);

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
