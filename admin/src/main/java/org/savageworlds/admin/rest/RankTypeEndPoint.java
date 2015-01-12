package org.savageworlds.admin.rest;

import javax.enterprise.context.RequestScoped;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import org.savageworlds.admin.dto.RankTypeDto;
import org.savageworlds.admin.dto.RankTypeList;
import org.savageworlds.game.model.RankType;

@RequestScoped
@Path("/rankTypes")
public class RankTypeEndPoint {

	@GET
	@Produces("application/json")
	public RankTypeList listAll() {
		RankTypeList list = new RankTypeList();
		for( RankType rankType : RankType.values()) {
			list.add( new RankTypeDto( rankType.name(), rankType.name(), rankType.ordinal()));
		}
		return list;
	}
	
	@GET
	@Path("/{id}")
	@Produces("application/json")
	public RankTypeDto findById(@PathParam("id") final String id) {
		RankType type = RankType.valueOf(id);
		return new RankTypeDto( type.name(), type.name(), type.ordinal());
	}
}
