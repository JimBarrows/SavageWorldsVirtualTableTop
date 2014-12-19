package org.savageworlds.game.model;

import java.io.Serializable;
import java.lang.Integer;
import java.lang.Long;
import java.lang.String;
import java.util.HashSet;
import java.util.Set;

import javax.persistence.*;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entity implementation class for Entity: Power
 *
 */
@Entity
@XmlRootElement
public class Power implements Serializable {

	@Id
	@GeneratedValue
	private Long				id;
	private String				name;
	@Lob
	private String				description;
	private RankType			rank;
	private Integer				powerPoints;
	private String				range;
	private String				duration;
	@OneToMany
	private Set<Trapping>		trappings			= new HashSet<Trapping>();
	private static final long	serialVersionUID	= 1L;

	public Power() {
		super();
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Long getId() {
		return this.id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public RankType getRank() {
		return this.rank;
	}

	public void setRank(RankType rank) {
		this.rank = rank;
	}

	public Integer getPowerPoints() {
		return this.powerPoints;
	}

	public void setPowerPoints(Integer powerPoints) {
		this.powerPoints = powerPoints;
	}

	public String getRange() {
		return this.range;
	}

	public void setRange(String range) {
		this.range = range;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
	}

	public Set<Trapping> getTrappings() {
		return trappings;
	}

	public void setTrappings(Set<Trapping> trappings) {
		this.trappings = trappings;
	}

}
