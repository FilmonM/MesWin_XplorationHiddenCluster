// !preview r2d3 data=readr::read_tsv("Archiv/Beispiel.tsv"), d3_version = "5", container = "div"
//r2d3: https://rstudio.github.io/r2d3, d3_version=3
// d3.tip
// Returns a tip
/* !preview r2d3 data=readr::read_tsv("Beispiel.tsv"), d3_version = "3", container = "div"*/
/* !preview r2d3 data=readr::read_tsv("Histogram_Data_Barplot.tsv"), d3_version = "3", container = "div" */
// Function to create the tool tips for the barplot-----------------
d3.tip = function() {
  var direction   = d3TipDirection,
      offset      = d3TipOffset,
      html        = d3TipHTML,
      rootElement = document.body,
      node        = initNode(),
      svg         = null,
      point       = null,
      target      = null;

  function tip(vis) {
    svg = getSVGNode(vis);
    if (!svg) return;
    point = svg.createSVGPoint();
    rootElement.appendChild(node);
  }

  // Public - show the tooltip on the screen
  //
  // Returns a tip
  tip.show = function() {
    var args = Array.prototype.slice.call(arguments);
    if (args[args.length - 1] instanceof SVGElement) target = args.pop();

    var content = html.apply(this, args),
        poffset = offset.apply(this, args),
        dir     = direction.apply(this, args),
        nodel   = getNodeEl(),
        i       = directions.length,
        coords,
        scrollTop  = document.documentElement.scrollTop ||
      rootElement.scrollTop,
        scrollLeft = document.documentElement.scrollLeft ||
      rootElement.scrollLeft;

    nodel.html(content)
      .style('opacity', 1).style('pointer-events', 'all');

    while (i--) nodel.classed(directions[i], false);
    coords = directionCallbacks.get(dir).apply(this);
    nodel.classed(dir, true)
      .style('top', (coords.top + poffset[0]) + scrollTop + 'px')
      .style('left', (coords.left + poffset[1]) + scrollLeft + 'px');

    return tip;
  };

  // Public - hide the tooltip
  //
  // Returns a tip
  tip.hide = function() {
    var nodel = getNodeEl();
    nodel.style('opacity', 0).style('pointer-events', 'none');
    return tip;
  };

  // Public: Proxy attr calls to the d3 tip container.
  // Sets or gets attribute value.
  //
  // n - name of the attribute
  // v - value of the attribute
  //
  // Returns tip or attribute value
  // eslint-disable-next-line no-unused-vars
  tip.attr = function(n, v) {
    if (arguments.length < 2 && typeof n === 'string') {
      return getNodeEl().attr(n);
    }

    var args =  Array.prototype.slice.call(arguments);
    d3.selection.prototype.attr.apply(getNodeEl(), args);
    return tip;
  };

  // Public: Proxy style calls to the d3 tip container.
  // Sets or gets a style value.
  //
  // n - name of the property
  // v - value of the property
  //
  // Returns tip or style property value
  // eslint-disable-next-line no-unused-vars
  tip.style = function(n, v) {
    if (arguments.length < 2 && typeof n === 'string') {
      return getNodeEl().style(n);
    }

    var args = Array.prototype.slice.call(arguments);
    selection.prototype.style.apply(getNodeEl(), args);
    return tip;
  };

  // Public: Set or get the direction of the tooltip
  //
  // v - One of n(north), s(south), e(east), or w(west), nw(northwest),
  //     sw(southwest), ne(northeast) or se(southeast)
  //
  // Returns tip or direction
  tip.direction = function(v) {
    if (!arguments.length) return direction;
    direction = v == null ? v : functor(v)

    return tip;
  };

  // Public: Sets or gets the offset of the tip
  //
  // v - Array of [x, y] offset
  //
  // Returns offset or
  tip.offset = function(v) {
    if (!arguments.length) return offset;
    offset = v == null ? v : functor(v)

    return tip;
  };

  // Public: sets or gets the html value of the tooltip
  //
  // v - String value of the tip
  //
  // Returns html value or tip
  tip.html = function(v) {
    if (!arguments.length) return html;
    html = v == null ? v : functor(v)

    return tip;
  };

  // Public: sets or gets the root element anchor of the tooltip
  //
  // v - root element of the tooltip
  //
  // Returns root node of tip
  tip.rootElement = function(v) {
    if (!arguments.length) return rootElement;
    rootElement = v == null ? v : functor(v)

    return tip;
  };

  // Public: destroys the tooltip and removes it from the DOM
  //
  // Returns a tip
  tip.destroy = function() {
    if (node) {
      getNodeEl().remove()
      node = null;
    }
    return tip;
  };

  function d3TipDirection() { return 'n' }
  function d3TipOffset() { return [0, 0] }
  function d3TipHTML() { return ' ' }

  var directionCallbacks = d3.map({
        n:  directionNorth,
        s:  directionSouth,
        e:  directionEast,
        w:  directionWest,
        nw: directionNorthWest,
        ne: directionNorthEast,
        sw: directionSouthWest,
        se: directionSouthEast
      }),
      directions = directionCallbacks.keys();

  function directionNorth() {
    var bbox = getScreenBBox(this);
    return {
      top:  bbox.n.y - node.offsetHeight,
      left: bbox.n.x - node.offsetWidth / 2
    };
  }

  function directionSouth() {
    var bbox = getScreenBBox(this);
    return {
      top:  bbox.s.y,
      left: bbox.s.x - node.offsetWidth / 2
    };
  }

  function directionEast() {
    var bbox = getScreenBBox(this);
    return {
      top:  bbox.e.y - node.offsetHeight / 2,
      left: bbox.e.x
    };
  }

  function directionWest() {
    var bbox = getScreenBBox(this);
    return {
      top:  bbox.w.y - node.offsetHeight / 2,
      left: bbox.w.x - node.offsetWidth
    };
  }

  function directionNorthWest() {
    var bbox = getScreenBBox(this);
    return {
      top:  bbox.nw.y - node.offsetHeight,
      left: bbox.nw.x - node.offsetWidth
    }
  }

  function directionNorthEast() {
    var bbox = getScreenBBox(this)
    return {
      top:  bbox.ne.y - node.offsetHeight,
      left: bbox.ne.x
    }
  }

  function directionSouthWest() {
    var bbox = getScreenBBox(this)
    return {
      top:  bbox.sw.y,
      left: bbox.sw.x - node.offsetWidth
    }
  }

  function directionSouthEast() {
    var bbox = getScreenBBox(this)
    return {
      top:  bbox.se.y,
      left: bbox.se.x
    }
  }

  function initNode() {
    var div = d3.select(document.createElement('div'))
    div
      .style('position', 'absolute')
      .style('top', 0)
      .style('opacity', 0)
      .style('pointer-events', 'none')
      .style('box-sizing', 'border-box')

    return div.node()
  }

  function getSVGNode(element) {
    var svgNode = element.node()
    if (!svgNode) return null
    if (svgNode.tagName.toLowerCase() === 'svg') return svgNode
    return svgNode.ownerSVGElement
  }

  function getNodeEl() {
    if (node == null) {
      node = initNode()
      // re-add node to DOM
      rootElement.appendChild(node)
    }
    return d3.select(node)
  }

  // Private - gets the screen coordinates of a shape
  //
  // Given a shape on the screen, will return an SVGPoint for the directions
  // n(north), s(south), e(east), w(west), ne(northeast), se(southeast),
  // nw(northwest), sw(southwest).
  //
  //    +-+-+
  //    |   |
  //    +   +
  //    |   |
  //    +-+-+
  //
  // Returns an Object {n, s, e, w, nw, sw, ne, se}
  function getScreenBBox(targetShape) {
    var targetel   = target || targetShape

    while (targetel.getScreenCTM == null && targetel.parentNode != null) {
      targetel = targetel.parentNode
    }

    var bbox       = {},
        matrix     = targetel.getScreenCTM(),
        tbbox      = targetel.getBBox(),
        width      = tbbox.width,
        height     = tbbox.height,
        x          = tbbox.x,
        y          = tbbox.y

    point.x = x
    point.y = y
    bbox.nw = point.matrixTransform(matrix)
    point.x += width
    bbox.ne = point.matrixTransform(matrix)
    point.y += height
    bbox.se = point.matrixTransform(matrix)
    point.x -= width
    bbox.sw = point.matrixTransform(matrix)
    point.y -= height / 2
    bbox.w = point.matrixTransform(matrix)
    point.x += width
    bbox.e = point.matrixTransform(matrix)
    point.x -= width / 2
    point.y -= height / 2
    bbox.n = point.matrixTransform(matrix)
    point.y += height
    bbox.s = point.matrixTransform(matrix)

    return bbox
  }

  // Private - replace D3JS 3.X d3.functor() function
  function functor(v) {
    return typeof v === 'function' ? v : function() {
      return v
    }
  }

  return tip
}


// Function to create a simple button------
d3.button = function() {

  var padding = 8,
      radius = 1,
      stdDeviation = 5,
      offsetX = 2,
      offsetY = 4;

  function my(selection) {
      
      selection.each(function(d, i) {
      var g = d3.select(this)
          .attr("id", "d3-button" + i)
          .attr("transform", "translate(" + d.x + "," + d.y + ")");

      var text = g.append("text").text(d.label);
      var bbox = text.node().getBBox();
      var rect = g.insert("rect", "text")
          .attr("x", bbox.x - padding)
          .attr("y", bbox.y - padding)
          .attr("width", bbox.width + 2 * padding)
          .attr("height", bbox.height + 2 * padding)
          .attr("rx", radius)
          .attr("ry", radius)
          .on("mouseover", mouseover)
          .on("mouseout", mouseout)
          .on("click", click)
        
    });
      
  }

  function mouseover() { d3.select(this.parentNode).select("rect").classed("active", true) }
  function mouseout() { d3.select(this.parentNode).select("rect").classed("active", false) }
  function click(d, i) { d.function(); }

  return my;
    
}


//d3----------------------------------
    
var margin = {top: 40, right: 20, bottom: 30, left: 40},
/// width and height will be generated by R automatically. Removed parts: Numbers after width = xxx and height = xxx.
    width = width - margin.left - margin.right,
    height = height - margin.top - margin.bottom;

var formatPercent = d3.format("");

var x = d3.scaleBand()
    .rangeRound([0, width])
    .padding(.1);

var y = d3.scaleLinear()
    .rangeRound([height, 0]);

var xAxis = d3.axisBottom()
    .scale(x);
    
var yAxis = d3.axisLeft()
    .scale(y)
//    .tickFormat(formatPercent);

var tip = d3.tip()
  .attr('class', 'd3-tip')
  .offset([-10, 0])
  .html(function(d) {
    return "<strong>SubCluster:</strong> <span style='color:red'>" + d.SubCluster + "</span>";
  })

//R already created a SVG element. So we do not need to create it again. Removing the following part:
//d3.select("body")
var svg = div.append("svg")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
  .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

svg.call(tip);

//function to update the plot with new input

function update(data){
  var u = svg.selectAll(".bar")
  .data(data)
  
    x.domain(data.map(function(d) { return d.DimensionName; }));
  y.domain([0, d3.max(data, function(d) { return d.Amount;})]);
  //d3.count(data, d => d.amount);

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Occurence");

  
  u
  .enter()
  .append("rect")
  .attr("class", "bar")
  .attr("x", function(d) { return x(d.DimensionName); })
  .attr("width", x.bandwidth())
  .attr("y", function(d) { return y(d.Amount); })
  .attr("height", function(d) { return height - y(d.Amount); })
  .on('mouseover', tip.show)
  .on('mouseout', tip.hide);
  
}

//Color gradient
var colors = ['rgba(167,207,223,1)', 'rgba(35,83,138,1)']
var gradient = svg.append('defs')
  .append('linearGradient')
  .attr('id', 'gradient')
  .attr('x1', '0%')
  .attr('x2', '0%')
  .attr('y1', '0%')
  .attr('y2', '100%');

gradient.selectAll('stop')
  .data(colors)
  .enter()
  .append('stop')
  .style('stop-color', function(d){ return d; })
  .attr('offset', function(d,i){
    return 100 * (i / (colors.length - 1)) + '%';
  })


//Create Button
var button = d3.button(); 
var fnA = function(){ Shiny.setInputValue("bar_general_clicked", null); }
// create data for button
var databtn = [{label: "Reset", x: 1, y: -22, function: function(){ fnA(); } }];



// To make the data input dynamically, we would do this: Create .tsv file (only in this example)
// Then change the header to read this specific .tsv file
// r2d3.onRender = To read the data into r2d3. 
r2d3.onRender(function(data, s, w, h, options) {
  x.domain(data.map(function(d) { return d.DimensionName; }));
  y.domain([0, d3.max(data, function(d) { return d.Amount;})]);
    svg.selectAll("g").remove();
    svg.selectAll(".bar").remove();
  //d3.count(data, d => d.amount);

  svg.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + height + ")")
      .call(xAxis);

  svg.append("g")
      .attr("class", "y axis")
      .call(yAxis)
    .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", 6)
      .attr("dy", ".71em")
      .style("text-anchor", "end")
      .text("Occurence");


    var buttons = svg.selectAll(".button")
  .data(databtn)
  .enter()
  .append("g")
  .attr("class", "button")
  .on('click', (d) => Shiny.setInputValue("bar_general_clicked", null))
  .call(button)
  ;

  svg.selectAll(".bar")
      .data(data)
    .enter()
    .append("rect")
      .attr("class", "bar")
      .attr("x", function(d) { return x(d.DimensionName); })
      .attr("width", x.bandwidth())
      .attr("y", function(d) { return y(d.Amount); })
      .attr("height", function(d) { return height - y(d.Amount); })
      .on('mouseover', tip.show)
      .on('mouseout', tip.hide)
      .on('click', (d) => Shiny.setInputValue("bar_general_clicked", d.DimensionName))
          ;
      
/*      .on('click', function(){
        Shiny.setInputValue('bar_clicked', d3.select(this).attr("x", function(d) { return x(d.DimensionName); })
       , {priority: 'event'});
    }) */
     //.remov1e()
     ;
  
/*svg.select(".bar")
    .transition()
    .duration(500)
    .attr("x", function(d) { return x(d.DimensionName); })
    .attr("width", function(d) { return y(d.Amount); })
    .attr("y")
    .remove(); */
      
    
      
    
// svg.exit().remove();
      
/*  svg.transition()
  .data(data)
  .duration(500)
      .attr("class", "bar")
      .attr("x", function(d) { return x(d.DimensionName); })
      .attr("width", x.bandwidth())
      .attr("y", function(d) { return y(d.Amount); })
      .attr("height", function(d) { return height - y(d.Amount); })
      .on('mouseover', tip.show)
      .on('mouseout', tip.hide)
      
svg.exit().remove() */
    
});


function type(d) {
  d.Amount = +d.Amount;
  return d;



}