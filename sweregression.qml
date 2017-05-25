<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis version="2.18.3" minimumScale="inf" maximumScale="1e+08" hasScaleBasedVisibilityFlag="0">
  <pipe>
    <rasterrenderer opacity="1" alphaBand="0" classificationMax="0.5" classificationMinMaxOrigin="User" band="1" classificationMin="0.101175" type="singlebandpseudocolor">
      <rasterTransparency/>
      <rastershader>
        <colorrampshader colorRampType="DISCRETE" clip="1">
          <item alpha="255" value="0.1" label="&lt;= .1" color="#f1eef6"/>
          <item alpha="255" value="0.25" label=".1 - .25" color="#bdc9e1"/>
          <item alpha="255" value="0.5" label=".25 - .5" color="#74a9cf"/>
          <item alpha="255" value="0.75" label=".5 - 0.75" color="#2b8cbe"/>
          <item alpha="255" value="2" label=".75 - 2" color="#045a8d"/>
          <item alpha="255" value="255" label="cloud" color="#fdbf6f"/>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast brightness="0" contrast="0"/>
    <huesaturation colorizeGreen="128" colorizeOn="0" colorizeRed="255" colorizeBlue="128" grayscaleMode="0" saturation="0" colorizeStrength="100"/>
    <rasterresampler maxOversampling="2"/>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
