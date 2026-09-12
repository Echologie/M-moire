/* Orthographic XYZ projection. The DOM preserves sharp, accessible mathematics.
   Elm owns the coordinates; the surface only proposes movements on the active plane. */
import { axes } from './session.js';
const SVG = 'http://www.w3.org/2000/svg';
const reduced = () => matchMedia('(prefers-reduced-motion: reduce)').matches;
const colors = { x: '#087f71', y: '#087ea2', z: '#7260b3' }, xyz = ['x', 'y', 'z'];
const point = (x = 0, y = 0, z = 0) => ({ x, y, z });
const dot = (a, b) => a.x * b.x + a.y * b.y + a.z * b.z;
const lerp = (a, b, t) => a + (b - a) * t;
const round = n => Math.round(Math.max(-10, Math.min(10, n)) * 10) / 10;
const bases = { xy: [point(1, 0, 0), point(0, 1, 0), point(0, 0, 1)], xz: [point(1, 0, 0), point(0, 0, 1), point(0, -1, 0)], yz: [point(0, 1, 0), point(0, 0, 1), point(1, 0, 0)] };
function orbitBasis(a, e) { return [point(Math.cos(a), 0, -Math.sin(a)), point(-Math.sin(a) * Math.sin(e), Math.cos(e), -Math.cos(a) * Math.sin(e)), point(Math.sin(a) * Math.cos(e), Math.sin(e), Math.cos(a) * Math.cos(e))]; }
function svg(name, attrs = {}) { const n = document.createElementNS(SVG, name); for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v); return n; }
const signed = n => n > 0 ? `+${n}` : String(n).replace('-', '−');

export class EvaluationSpace extends HTMLElement {
  static get observedAttributes() { return ['payload']; }
  connectedCallback() {
    this.azimuth = .64; this.elevation = .38; this.basis = orbitBasis(this.azimuth, this.elevation); this.cards = new Map();
    this.svg = svg('svg', { class: 'space-svg', 'aria-hidden': 'true' });
    this.walls = svg('g'); this.lines = svg('g'); this.guides = svg('g'); this.svg.append(this.walls, this.lines, this.guides);
    this.labels = document.createElement('div'); this.labels.className = 'axis-labels';
    this.orbs = document.createElement('div'); this.orbs.className = 'orb-layer';
    this.hint = document.createElement('div'); this.hint.className = 'projection-note'; this.append(this.svg, this.labels, this.orbs, this.hint);
    this.resize = new ResizeObserver(() => this.draw()); this.resize.observe(this);
    this.down = e => this.pointerDown(e); this.move = e => this.pointerMove(e); this.up = e => this.pointerUp(e);
    this.addEventListener('pointerdown', this.down); this.addEventListener('pointermove', this.move); this.addEventListener('pointerup', this.up); this.addEventListener('pointercancel', this.up); this.addEventListener('lostpointercapture', this.up);
    this.readPayload();
  }
  attributeChangedCallback() { if (this.cards) this.readPayload(); }
  disconnectedCallback() {
    this.resize?.disconnect(); cancelAnimationFrame(this.animation);
    this.removeEventListener('pointerdown', this.down); this.removeEventListener('pointermove', this.move); this.removeEventListener('pointerup', this.up); this.removeEventListener('pointercancel', this.up); this.removeEventListener('lostpointercapture', this.up);
  }
  orb(id) { return this.cards.get(id); }
  dispatch(name, detail) { this.dispatchEvent(new CustomEvent(name, { detail, bubbles: true })); }
  readPayload() {
    let next; try { next = JSON.parse(this.getAttribute('payload')); } catch { return; }
    const oldPlane = this.data?.plane; this.data = next;
    this.setAttribute('aria-label', next.plane === '3d' ? 'Espace à trois dimensions. Faites glisser le fond pour tourner.' : 'Plan de comparaison. Déplacez une bille avec le doigt, la souris ou les flèches du clavier.');
    this.classList.toggle('free-view', next.plane === '3d');
    for (const [id, orb] of this.cards) if (!next.points.some(p => p.id === id)) { orb.remove(); this.cards.delete(id); }
    for (const p of next.points) {
      let orb = this.cards.get(p.id);
      if (!orb) {
        orb = document.createElement('button'); orb.type = 'button'; orb.className = 'orb'; orb.dataset.id = p.id;
        const preview = document.createElement('rich-text'); preview.setAttribute('content', p.content); preview.className = 'orb-preview'; preview.setAttribute('aria-hidden', 'true');
        const shine = document.createElement('span'); shine.className = 'orb-shine';
        const title = document.createElement('span'); title.className = 'orb-caption'; title.innerHTML = `<small>Rédaction</small><strong>${p.number}</strong>`;
        orb.append(preview, shine, title);
        orb.onclick = e => { if (e.detail === 0) this.dispatch('read', { id: p.id }); };
        orb.onkeydown = e => this.key(e, p.id); this.cards.set(p.id, orb); this.orbs.append(orb);
      }
      orb.classList.toggle('selected', p.id === next.selected); orb.classList.toggle('pending', p.judged.length < 3); orb.classList.toggle('being-read', p.id === next.selected && next.reader);
      orb.setAttribute('aria-label', `Rédaction ${p.number}. ${xyz.map(a => `${axes[a].positive} ${p.judged.includes(a) ? signed(p.point[a]) : 'à placer'}`).join(', ')}. Entrée pour lire.`);
      orb.setAttribute('aria-keyshortcuts', 'Enter ArrowUp ArrowDown ArrowLeft ArrowRight');
    }
    if (oldPlane !== next.plane) this.turn(next.plane); else this.draw();
  }
  turn(plane) {
    cancelAnimationFrame(this.animation);
    const from = this.basis, target = plane === '3d' ? orbitBasis(this.azimuth, this.elevation) : bases[plane];
    if (reduced()) { this.basis = target; this.draw(); return; }
    const started = performance.now();
    const tick = now => {
      const t = Math.min(1, (now - started) / 500), k = 1 - Math.pow(1 - t, 3);
      this.basis = from.map((v, i) => point(lerp(v.x, target[i].x, k), lerp(v.y, target[i].y, k), lerp(v.z, target[i].z, k)));
      this.draw(); if (t < 1) this.animation = requestAnimationFrame(tick);
    }; this.animation = requestAnimationFrame(tick);
  }
  project(p) { return { x: this.cx + dot(p, this.basis[0]) * this.unit, y: this.cy - dot(p, this.basis[1]) * this.unit, depth: dot(p, this.basis[2]) }; }
  line(parent, a, b, attributes = {}) { const p = this.project(a), q = this.project(b); parent.append(svg('line', { x1: p.x, y1: p.y, x2: q.x, y2: q.y, ...attributes })); }
  draw() {
    if (!this.data || !this.clientWidth) return;
    const w = this.clientWidth, h = this.clientHeight, free = this.data.plane === '3d'; this.cx = w / 2; this.cy = h / 2;
    this.unit = Math.max(3, free ? Math.min((w - 120) / 34, (h - 86) / 31) : Math.min((w - 110) / 25, (h - 78) / 25));
    this.svg.setAttribute('viewBox', `0 0 ${w} ${h}`); this.walls.replaceChildren(); this.lines.replaceChildren(); this.guides.replaceChildren(); this.labels.replaceChildren();
    for (const pair of free ? ['xy', 'xz', 'yz'] : [this.data.plane]) {
      const [a, b] = pair, hidden = xyz.find(axis => !pair.includes(axis)), base = point(); base[hidden] = free ? -10 : 0;
      const corners = [[-10, -10], [10, -10], [10, 10], [-10, 10]].map(([u, v]) => this.project({ ...base, [a]: u, [b]: v }));
      this.walls.append(svg('polygon', { points: corners.map(p => `${p.x},${p.y}`).join(' '), fill: free ? colors[hidden] : '#dff1ed', 'fill-opacity': free ? '.045' : '.22', stroke: '#a8c8c2', 'stroke-opacity': '.65' }));
      for (let tick = -10; tick <= 10; tick += 2) {
        this.line(this.walls, { ...base, [a]: tick, [b]: -10 }, { ...base, [a]: tick, [b]: 10 }, { stroke: '#a9c6c1', 'stroke-width': '.7', 'stroke-opacity': '.34' });
        this.line(this.walls, { ...base, [a]: -10, [b]: tick }, { ...base, [a]: 10, [b]: tick }, { stroke: '#a9c6c1', 'stroke-width': '.7', 'stroke-opacity': '.34' });
      }
    }
    for (const axis of free ? xyz : [...this.data.plane]) {
      this.line(this.lines, { ...point(), [axis]: -10.5 }, { ...point(), [axis]: 10.5 }, { stroke: colors[axis], 'stroke-width': '1.5', 'stroke-opacity': '.7' });
      for (const value of [-10, -5, 0, 5, 10]) {
        const p = this.project({ ...point(), [axis]: value }); this.lines.append(svg('circle', { cx: p.x, cy: p.y, r: '2', fill: colors[axis], opacity: '.75' }));
        if (Math.abs(value) === 5) { const tick = svg('text', { x: p.x + 7, y: p.y + 14, fill: colors[axis], 'font-size': '11' }); tick.textContent = signed(value); this.lines.append(tick); }
      }
      for (const sign of [-1, 1]) {
        const p = this.project({ ...point(), [axis]: sign * (free ? 12.4 : 11.8) }); const label = document.createElement('span'); label.className = `axis-label axis-${axis}`; label.style.left = `${p.x}px`; label.style.top = `${p.y}px`;
        label.innerHTML = `<strong>${sign < 0 ? axes[axis].negative : axes[axis].positive}</strong><small>${sign < 0 ? '−10' : '+10'}</small>`; this.labels.append(label);
      }
    }
    const zero = this.project(point()); this.lines.append(svg('circle', { cx: zero.x, cy: zero.y, r: '3', fill: '#456962' }));
    const zeroLabel = svg('text', { x: zero.x + 7, y: zero.y + 15, fill: '#5f7973', 'font-size': '12' }); zeroLabel.textContent = '0'; this.lines.append(zeroLabel);
    const selected = this.data.points.find(p => p.id === this.data.selected);
    if (selected) {
      const pos = this.drag?.id === selected.id && this.drag.current ? this.drag.current : selected.point;
      if (free) for (const axis of xyz) {
        const foot = { ...pos, [axis]: -10 }; this.line(this.guides, pos, foot, { stroke: colors[axis], 'stroke-dasharray': '4 4', 'stroke-width': '1.3', opacity: '.65' }); const p = this.project(foot); this.guides.append(svg('circle', { cx: p.x, cy: p.y, r: '4', fill: colors[axis], opacity: '.3' }));
      } else for (const axis of this.data.plane) this.line(this.guides, pos, { ...pos, [axis]: 0 }, { stroke: colors[axis], 'stroke-dasharray': '4 4', 'stroke-width': '1.2', opacity: '.6' });
    }
    const projected = this.data.points.map(item => { const pos = this.drag?.id === item.id && this.drag.current ? this.drag.current : item.point; return { item, pos, screen: this.project(pos) }; });
    for (const { item, pos, screen } of projected) {
      const orb = this.cards.get(item.id);
      // Fan overlapping projections out, with a visible line to the exact coordinate.
      const siblings = projected.filter(p => Math.hypot(p.screen.x - screen.x, p.screen.y - screen.y) < 34).sort((a, b) => a.item.number - b.item.number), order = siblings.findIndex(p => p.item.id === item.id);
      const offsetX = siblings.length > 1 ? (order - (siblings.length - 1) / 2) * (w < 600 ? 60 : 82) : 0, offsetY = siblings.length > 1 ? -30 : 0;
      const x = Math.max(42, Math.min(w - 42, screen.x + offsetX)), y = Math.max(42, Math.min(h - 42, screen.y + offsetY));
      if (offsetX || offsetY || x !== screen.x || y !== screen.y) { this.guides.append(svg('line', { x1: screen.x, y1: screen.y, x2: x, y2: y, stroke: '#526f68', 'stroke-width': '1.1', 'stroke-dasharray': '3 3' })); this.guides.append(svg('circle', { cx: screen.x, cy: screen.y, r: '4', fill: '#087f71', stroke: 'white', 'stroke-width': '1.5' })); }
      const size = free ? Math.max(.78, Math.min(1.12, .94 + screen.depth / 110)) : 1;
      Object.assign(orb.style, { left: `${x}px`, top: `${y}px`, transform: `translate(-50%,-50%) scale(${size})`, zIndex: item.id === this.data.selected ? 80 : Math.round(screen.depth + 35) });
      orb.dataset.x = pos.x; orb.dataset.y = pos.y; orb.dataset.z = pos.z;
    }
    if (free) this.hint.textContent = 'Trois dimensions · les pointillés relient la bille aux trois faces';
    else { const hidden = xyz.find(a => !this.data.plane.includes(a)); this.hint.textContent = `${hidden === 'x' ? 'Lisibilité' : hidden === 'y' ? 'Précision' : 'Validité'} conservée · ${selected && selected.judged.includes(hidden) ? signed(selected.point[hidden]) : 'à placer sur une autre face'}`; }
  }
  pointerDown(e) {
    if (e.button !== 0 || this.data.reader || this.drag) return;
    const card = e.target.closest('.orb'), entry = card && this.data.points.find(p => p.id === card.dataset.id);
    this.drag = { pointer: e.pointerId, x: e.clientX, y: e.clientY, id: entry?.id, start: entry ? { ...entry.point } : null, moved: false, az: this.azimuth, el: this.elevation };
    this.setPointerCapture(e.pointerId); if (card) card.focus({ preventScroll: true });
  }
  pointerMove(e) {
    const d = this.drag; if (!d || e.pointerId !== d.pointer) return;
    const dx = e.clientX - d.x, dy = e.clientY - d.y; if (Math.hypot(dx, dy) > 5) d.moved = true; if (!d.moved) return;
    if (this.data.plane === '3d') { cancelAnimationFrame(this.animation); this.azimuth = d.az - dx * .008; this.elevation = Math.max(-1.25, Math.min(1.25, d.el + dy * .007)); this.basis = orbitBasis(this.azimuth, this.elevation); this.draw(); }
    else if (d.id) { const [a, b] = this.data.plane; d.current = { ...d.start, [a]: round(d.start[a] + dx / this.unit), [b]: round(d.start[b] - dy / this.unit) }; this.cards.get(d.id)?.classList.add('dragging'); this.draw(); }
  }
  pointerUp(e) {
    const d = this.drag; if (!d || (e.pointerId !== undefined && e.pointerId !== d.pointer)) return;
    this.drag = null; this.cards.get(d.id)?.classList.remove('dragging');
    if (e.type === 'pointercancel' || e.type === 'lostpointercapture') { this.draw(); return; }
    if (d.moved) { if (this.data.plane === '3d') this.dispatch('orbit', {}); else if (d.current) this.dispatch('placement', { id: d.id, point: d.current }); }
    else if (d.id) this.dispatch('read', { id: d.id }); this.draw();
  }
  key(e, id) {
    if (this.data.reader || this.data.plane === '3d') return;
    const motion = { ArrowLeft: [-1, 0], ArrowRight: [1, 0], ArrowUp: [0, 1], ArrowDown: [0, -1] }[e.key]; if (!motion) return;
    e.preventDefault(); const current = this.data.points.find(p => p.id === id), [a, b] = this.data.plane, step = e.shiftKey ? 1 : .1;
    // Keep successive key presses relative to the latest proposal until Elm paints its next frame.
    current.point = { ...current.point, [a]: round(current.point[a] + motion[0] * step), [b]: round(current.point[b] + motion[1] * step) };
    this.dispatch('placement', { id, point: current.point });
  }
}
customElements.define('evaluation-space', EvaluationSpace);
