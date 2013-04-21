<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
<script>
    var what = null;

    var increase = 38,
        decrease = 40;

    var initialSize;
    var userSize;

    document.onkeydown = function(ev) 
    {
        switch(ev.keyCode) 
        {
            case increase:
            case decrease:
                what = ev.keyCode;
                break;

            default:
                what = null;
                break;
        }
        if (what) return false;
    }

    document.onkeyup = function(ev) {
        what = null;
    }

    $(document).ready(function() {
        var canvas = $('#canv');
        var context = canvas.get(0).getContext('2d');
        var size = parseInt(document.getElementById('canv').getAttribute('data-initial'));
        var color = document.getElementById('canv').getAttribute('data-color');
        var shape = document.getElementById('canv').getAttribute('data-shape');

        initialSize = size;
        
        draw(context, color, shape, size, size);
    });

    function drawShape(context, color, shape, x, y, size) {
        if (shape == "Circle") {
            context.beginPath();
            context.arc(x, y, size/2, 0, 2 * Math.PI, false);
        } else if (shape == "Square") {
            context.rect(x-size/2, y-size/2, size, size);
        } else alert("shape error");
        context.fillStyle = color;
        context.fill();
        context.stroke();
        context.closePath();
    }
    
    function draw(context, color, shape, size1, size2) {
        context.beginPath();

        context.clearRect(0, 0, 800, 600);
        context.fillStyle = 'white';
        context.strokeStyle = 'black';
        context.fillRect(0, 0, 800, 600);

        drawShape(context, color, shape, 200, 300, size1);
        drawShape(context, color, shape, 600, 300, size2);

        var change;

        if (what == increase) change = 1;
        else if (what == decrease) change = -1;
        else change = 0;

        userSize = size2 + change;

        if (userSize <   1) userSize = 1;
        if (userSize > 400) userSize = 400;

        document.getElementById('userSize').value = userSize;

        setTimeout(function(){ draw(context, color, shape, size1, userSize) }, 10);
    }   
</script>

<ul class="nav nav-pills">
  <li><a href="/experiment">Skip</a></li>
  <li><a href="/">Finish</a></li>
  <li><a href="/logout">Logout</a></li>
</ul>

<p class="text-center">user: <loggedInUser/>, target ratio: <b><ratio/></b></p>

<form method="post" action="/experiment">
  <input type="hidden" name="initialSize" value=${iniSz}>
  <input type="hidden" name="color" value=${color}>
  <input type="hidden" name="ratio" value=${ratio}>
  <input type="hidden" name="shape" value=${shape}>
  <input type="hidden" id="userSize" name="userSize">
  <p class="text-center"><button type="submit" class="btn">Done!</button></p>
</form>

<div style="margin: 0 auto; width: 800px;">
  <canvas id="canv" width="800" height="600" data-initial=${iniSz} data-color=${color} data-shape=${shape}></canvas>
</div>


