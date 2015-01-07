package org.savageworlds.admin.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.NotEmpty;
import org.savageworlds.game.model.CharacterType;
import org.savageworlds.game.model.EdgeDescription;
import org.savageworlds.game.model.RankType;
import org.savageworlds.game.model.Skill;

import com.fasterxml.jackson.annotation.JsonRootName;

@JsonRootName("edgeDescription")
public class EdgeDescriptionDto implements Serializable {

	/**
	 * 
	 */
	private static final long			serialVersionUID	= 1L;

	private Long									id;

	private Long									version						= 0l;

	@NotNull
	private Long									edgeType					= null;

	@NotEmpty
	private String								name;

	private RankType							minimumRank				= RankType.Novice;

	@NotNull
	private CharacterType					requiredType			= CharacterType.Extra;

	private List<Skill>						minimumSkills			= null;

	private List<EdgeDescription>	requiredEdges			= null;

	public EdgeDescriptionDto() {
		super();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
	}

	public Long getEdgeType() {
		return edgeType;
	}

	public void setEdgeType(Long edgeType) {
		this.edgeType = edgeType;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public RankType getMinimumRank() {
		return minimumRank;
	}

	public void setMinimumRank(RankType minimumRank) {
		this.minimumRank = minimumRank;
	}

	public CharacterType getRequiredType() {
		return requiredType;
	}

	public void setRequiredType(CharacterType requiredType) {
		this.requiredType = requiredType;
	}

	public List<Skill> getMinimumSkills() {
		return minimumSkills;
	}

	public void setMinimumSkills(List<Skill> minimumSkills) {
		this.minimumSkills = minimumSkills;
	}

	public List<EdgeDescription> getRequiredEdges() {
		return requiredEdges;
	}

	public void setRequiredEdges(List<EdgeDescription> requiredEdges) {
		this.requiredEdges = requiredEdges;
	}

}
