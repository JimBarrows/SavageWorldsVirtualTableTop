package org.savageworlds.plotpoint.dto;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlRootElement;

import org.savageworlds.plotpoint.model.PlotPoint;

import com.fasterxml.jackson.annotation.JsonRootName;

@XmlRootElement(name="PlotPoints")
@JsonRootName("PlotPoints")
public class PlotPointList extends ArrayList<PlotPoint> {	
	
	
	private static final long	serialVersionUID	= 1L;
}
