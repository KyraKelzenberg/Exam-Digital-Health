r2d3.onRender(function(data, svg, width, height) {
 
  svg.selectAll("*").remove();
 
  width = width || 960;
  height = height || 600;
 
  var projection = d3.geoMercator()
    .scale(width / 6.5)
    .translate([width/2, height/1.5]);
 
  var path = d3.geoPath().projection(projection);
 
  // Tooltip
  var tooltip = d3.select("body")
    .append("div")
    .attr("class","tooltip")
    .style("position","absolute")
    .style("pointer-events","none")
    .style("background","white")
    .style("border","1px solid #ccc")
    .style("padding","4px")
    .style("opacity",0);
 
  // Draw countries
  svg.selectAll("path")
    .data(data.world.features)
    .enter()
    .append("path")
    .attr("d", path)
    .attr("fill","lightgray")
    .attr("stroke","white")
    .attr("stroke-width",1)
    .on("mouseenter", function(event,d){
      var origin_iso = d.properties.iso_a3;
      var flows = data.flows.filter(f => f.country_of_origin_iso === origin_iso);
 
      var lines = svg.selectAll(".flow").data(flows, f => f.country_of_asylum_iso);
 
      lines.enter()
        .append("path")
        .attr("class","flow")
        .merge(lines)
        .attr("fill","none")
        .attr("stroke","steelblue")
        .attr("stroke-width", f => Math.log1p(f.refugees))
        .attr("opacity",0.7)
        .attr("d", function(f){
          var start = projection([f.x_start,f.y_start]);
          var end = projection([f.x_end,f.y_end]);
          var midX = (start[0]+end[0])/2;
          return "M"+start[0]+","+start[1]+
                 "C"+midX+","+start[1]+" "+midX+","+end[1]+" "+end[0]+","+end[1];
        })
        .on("mousemove", function(event,f){
          tooltip.style("left",(event.pageX+10)+"px")
                 .style("top",(event.pageY+10)+"px")
                 .style("opacity",1)
                 .html(f.country_of_origin + " → " + f.country_of_asylum + ": " + f.refugees);
        })
        .on("mouseout", function(){
          tooltip.style("opacity",0);
        });
 
      lines.exit().remove();
    })
    .on("mouseleave", function(){
      svg.selectAll(".flow").remove();
      tooltip.style("opacity",0);
    });
 
});