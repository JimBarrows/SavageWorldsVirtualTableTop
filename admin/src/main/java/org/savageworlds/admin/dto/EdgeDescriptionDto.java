package org.savageworlds.admin.dto;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.OneToMany;

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
	private static final long	serialVersionUID	= 1L;

	private Long									id;

	private Long									version				= 0l;

	private Long									edgeType			= null;

	@NotEmpty
	private String								name;

	private RankType							minimumRank		= RankType.Novice;

	private CharacterType					requiredType	= CharacterType.Extra;

	@OneToMany
	private Set<Skill>						minimumSkills	= new HashSet<Skill>();

	@OneToMany
	private Set<EdgeDescription>	requiredEdges	= new HashSet<EdgeDescription>();
	
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

	public Set<Skill> getMinimumSkills() {
		return minimumSkills;
	}

	public void setMinimumSkills(Set<Skill> minimumSkills) {
		this.minimumSkills = minimumSkills;
	}

	public Set<EdgeDescription> getRequiredEdges() {
		return requiredEdges;
	}

	public void setRequiredEdges(Set<EdgeDescription> requiredEdges) {
		this.requiredEdges = requiredEdges;
	}

}
