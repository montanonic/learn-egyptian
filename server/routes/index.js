var express = require('express');
var fs = require('fs');
var path = require('path');
var router = express.Router();

/* GET home page. */
router.get('/', function (req, res, next) {
  res.render('index', { title: 'Express' });
});

// I can add the ability to server-side save, soon.
// router.get('/initializeData', function (req, res, next) {
//   var rawData = fs.readFileSync(path.join(__dirname, '../data/backup_19.json'));
//   res.send(JSON.parse(rawData));
// });

router.post('/updateAudioName', function (req, res, next) {
  const { old, new: new_, type } = req.body;
  const audioPath = '/home/montanonic/elm/egyptian/server/public/audio';
  const fileType = type || "wav";
  // Ah okay, the last thing we need to know is the file type. This will differ depending on that.
  // We'll handle this in audio upload.
  fs.renameSync(`${audioPath}/${old}.${fileType}`, `${audioPath}/${new_}.${fileType}`)
  res.send('file updated')
});

module.exports = router;
